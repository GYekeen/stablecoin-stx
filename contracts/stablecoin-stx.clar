;; stablecoin-stx.clar - Collateralized stablecoin backed by STX collateral
;; - Users lock STX, mint USDX (fungible token defined here)
;; - Accrues stability fee (simple per-block)
;; - Oracle provides STX price in microUSD (1e6) units
;; - Liquidation when CR < threshold, with penalty to borrower and bonus for liquidator
;; - All amounts use 6 decimals (microUSDX) to match price scale

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define error codes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_BAD_AMOUNT u101)
(define-constant ERR_PRICE u102)
(define-constant ERR_HEALTH u103)
(define-constant ERR_UNDERCOLL u104)
(define-constant ERR_NOT_UNDERWATER u105)
(define-constant ERR_NO_LIQUIDITY u106)
(define-constant ERR_OVERPAY u107)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-constant BPS u10000)
(define-constant PRICE_SCALE u1000000)           ;; 1e6 microUSD per STX
(define-constant R_SCALE u1000000000000000000)   ;; 1e18 fixed-point for rates
(define-data-var current-block uint u0)          ;; Current block height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define oracle trait
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-trait oracle-trait
  ((get-price () (response uint uint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define contract dependencies
(define-constant CONTRACT_OWNER tx-sender)

;; Define price oracle function (mock for testing)
(define-read-only (get-mock-price)
  (ok u1000000))  ;; $1.00 in microUSD

;; Token: USDX (mint/burn controlled by this contract)
(define-fungible-token usdx)

;; Admin state
(define-data-var admin principal tx-sender)
(define-data-var oracle-principal principal tx-sender)

;; Risk parameters
(define-data-var min-cr-bps uint u15000)        ;; 150% minimum collateralization
(define-data-var liq-cr-bps uint u13000)        ;; 130% liquidation threshold
(define-data-var liq-penalty-bps uint u800)     ;; 8% penalty added to liquidated debt
(define-data-var mint-fee-bps uint u20)         ;; 0.20% mint fee
(define-data-var stability-rate-per-block uint u0)

;; Events
(define-data-var event-counter uint u0)

;; Vault state - stores collateral and debt
(define-map vaults
  { user: principal }
  { collateral: uint, debt: uint, last-update: uint })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-private (emit-deposit-event (user principal) (amount uint))
  (begin
    (var-set event-counter (+ (var-get event-counter) u1))
    (print "deposit")
    (print {user: user, amount: amount, id: (var-get event-counter)})))

(define-private (emit-withdraw-event (user principal) (amount uint))
  (begin
    (var-set event-counter (+ (var-get event-counter) u1))
    (print "withdraw")
    (print {user: user, amount: amount, id: (var-get event-counter)})))

(define-private (emit-mint-event (user principal) (amount uint) (fee uint))
  (begin
    (var-set event-counter (+ (var-get event-counter) u1))
    (print "mint")
    (print {user: user, amount: amount, fee: fee, id: (var-get event-counter)})))

(define-private (emit-repay-event (user principal) (amount uint))
  (begin
    (var-set event-counter (+ (var-get event-counter) u1))
    (print "repay")
    (print {user: user, amount: amount, id: (var-get event-counter)})))

(define-private (emit-liquidate-event (user principal) (repay-amount uint) (collateral-seized uint) (liquidator principal))
  (begin
    (var-set event-counter (+ (var-get event-counter) u1))
    (print "liquidate")
    (print {user: user, repay: repay-amount, collateral-seized: collateral-seized, liquidator: liquidator, id: (var-get event-counter)})))

(define-read-only (get-vault (who principal))
  (map-get? vaults { user: who }))

(define-read-only (get-admin) 
  (var-get admin))

(define-read-only (params)
  { oracle: (var-get oracle-principal),
    min-cr-bps: (var-get min-cr-bps),
    liq-cr-bps: (var-get liq-cr-bps),
    liq-penalty-bps: (var-get liq-penalty-bps),
    mint-fee-bps: (var-get mint-fee-bps),
    stability-rate-per-block: (var-get stability-rate-per-block) })

(define-read-only (is-admin (p principal)) 
  (is-eq p (var-get admin)))

(define-read-only (stx-price)
  (unwrap! (get-mock-price) ERR_PRICE))

(define-read-only (collateral-value-usd (coll uint) (price uint))
  (/ (* coll price) PRICE_SCALE))

(define-read-only (cr-bps-of (coll uint) (debt uint) (price uint))
  (if (is-eq debt u0)
      u18446744073709551615  ;; effectively infinite CR
      (/ (* (collateral-value-usd coll price) BPS) debt)))

(define-private (accrue (who principal))
  (ok 
    (let ((vault (map-get? vaults { user: who })))
      (if (is-none vault)
          true
          (let ((v (unwrap-panic vault))
                (debt (get debt v))
                (last (get last-update v))
                (now (var-get current-block))
                (rate (var-get stability-rate-per-block)))
            (if (or (is-eq debt u0) (is-eq rate u0) (<= now last))
                true
                (begin
                  (let ((delta (- now last))
                        (accrue-amt (/ (* debt (* rate delta)) R_SCALE)))
                    (map-set vaults { user: who }
                      { collateral: (get collateral v),
                        debt: (+ debt accrue-amt),
                        last-update: now }))
                  true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (var-set admin new-admin)
    (ok true)))

(define-public (set-oracle (o principal))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (var-set oracle-principal o)
    (ok true)))

(define-public (set-risk
  (min-cr uint) (liq-cr uint) (penalty-bps uint) (mint-fee uint) (rate-per-block uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (>= min-cr liq-cr) (err ERR_UNAUTHORIZED))
    (asserts! (< liq-cr BPS) (err ERR_UNAUTHORIZED))
    (asserts! (<= penalty-bps BPS) (err ERR_UNAUTHORIZED))
    (asserts! (<= mint-fee BPS) (err ERR_UNAUTHORIZED))
    (var-set min-cr-bps min-cr)
    (var-set liq-cr-bps liq-cr)
    (var-set liq-penalty-bps penalty-bps)
    (var-set mint-fee-bps mint-fee)
    (var-set stability-rate-per-block rate-per-block)
    (ok true)))

(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (unwrap! (accrue tx-sender) (err ERR_HEALTH))
    (let ((v (map-get? vaults { user: tx-sender })))
      (if (is-some v)
          (let ((vault (unwrap-panic v)))
            (map-set vaults { user: tx-sender }
              { collateral: (+ (get collateral vault) amount),
                debt: (get debt vault),
                last-update: (var-get current-block) }))
          (map-set vaults { user: tx-sender }
            { collateral: amount, debt: u0, last-update: (var-get current-block) })))
    (emit-deposit-event tx-sender amount)
    (ok true)))

(define-public (withdraw (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (unwrap! (accrue tx-sender) (err ERR_HEALTH))
    (let ((v (map-get? vaults { user: tx-sender })))
      (let ((vault (unwrap! v (err ERR_BAD_AMOUNT)))
            (curr-coll (get collateral vault)))
        (asserts! (>= curr-coll amount) (err ERR_BAD_AMOUNT))
        (let ((new-coll (- curr-coll amount))
              (curr-debt (get debt vault)))
          (if (> curr-debt u0)
              (begin
                (let ((p (stx-price))
                      (cr (cr-bps-of new-coll curr-debt p)))
                  (asserts! (>= cr (var-get min-cr-bps)) (err ERR_HEALTH)))
                true)
              true)
          (map-set vaults { user: tx-sender }
            { collateral: new-coll, debt: curr-debt, last-update: (var-get current-block) })
          (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
          (emit-withdraw-event tx-sender amount)
          (ok true))))))

(define-public (mint (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (unwrap! (accrue tx-sender) (err ERR_HEALTH))
    (let ((v (map-get? vaults { user: tx-sender })))
      (let ((vault (unwrap! v (err ERR_UNDERCOLL)))
            (fee (/ (* amount (var-get mint-fee-bps)) BPS))
            (new-debt (+ (get debt vault) amount fee))
            (p (stx-price))
            (cr (cr-bps-of (get collateral vault) new-debt p)))
        (asserts! (>= cr (var-get min-cr-bps)) (err ERR_HEALTH))
        (map-set vaults { user: tx-sender }
          { collateral: (get collateral vault),
            debt: new-debt,
            last-update: (var-get current-block) })
        (try! (ft-mint? usdx amount tx-sender))
        (emit-mint-event tx-sender amount fee)
        (ok { minted: amount, fee: fee })))))

(define-public (repay (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (unwrap! (accrue tx-sender) (err ERR_HEALTH))
    (let ((v (map-get? vaults { user: tx-sender })))
      (let ((vault (unwrap! v (err ERR_UNDERCOLL)))
            (debt (get debt vault))
            (pay (if (> amount debt) debt amount)))
        (asserts! (>= amount pay) (err ERR_OVERPAY))
        (try! (ft-burn? usdx pay tx-sender))
        (map-set vaults { user: tx-sender }
          { collateral: (get collateral vault),
            debt: (- debt pay),
            last-update: (var-get current-block) })
        (emit-repay-event tx-sender pay)
        (ok pay)))))

(define-public (liquidate (target-user principal) (repay-amount uint))
  (begin
    (asserts! (> repay-amount u0) (err ERR_BAD_AMOUNT))
    (asserts! (not (is-eq tx-sender target-user)) (err ERR_UNAUTHORIZED))
    (unwrap! (accrue target-user) (err ERR_HEALTH))
    (let ((v (map-get? vaults { user: target-user })))
      (let ((vault (unwrap! v (err ERR_UNDERCOLL)))
            (p (stx-price))
            (debt (get debt vault)))
        ;; must be underwater: CR < liq-cr-bps
        (let ((cr (cr-bps-of (get collateral vault) debt p)))
          (asserts! (< cr (var-get liq-cr-bps)) (err ERR_NOT_UNDERWATER)))
        (let ((actual (if (> repay-amount debt) debt repay-amount))
              (penalty (var-get liq-penalty-bps))
              (base (/ (* actual BPS) p))
              (seize (/ (* base (+ BPS penalty)) BPS)))
          (asserts! (>= (get collateral vault) seize) (err ERR_NO_LIQUIDITY))
          (try! (ft-burn? usdx actual tx-sender))
          (map-set vaults { user: target-user }
            { collateral: (- (get collateral vault) seize),
              debt: (- (get debt vault) actual),
              last-update: (var-get current-block) })
          (try! (stx-transfer? seize (as-contract tx-sender) tx-sender))
          (emit-liquidate-event target-user actual seize tx-sender)
          (ok { repaid: actual, seized: seize }))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-current-price) 
  (stx-price))

(define-read-only (collateral-of (who principal))
  (let ((v (map-get? vaults { user: who })))
    (if (is-some v) 
        (get collateral (unwrap-panic v)) 
        u0)))

(define-read-only (debt-of (who principal))
  (let ((v (map-get? vaults { user: who })))
    (if (is-some v) 
        (get debt (unwrap-panic v)) 
        u0)))

(define-read-only (cr-of (who principal))
  (let ((v (map-get? vaults { user: who }))
        (p (stx-price)))
    (if (is-some v)
        (cr-bps-of (get collateral (unwrap-panic v)) (get debt (unwrap-panic v)) p)
        u18446744073709551615)))

(define-read-only (max-mintable (who principal))
  (let ((v (map-get? vaults { user: who })))
    (if (is-none v)
        u0
        (let ((vault (unwrap-panic v))
              (p (stx-price))
              (mincr (var-get min-cr-bps))
              (coll (get collateral vault))
              (currDebt (get debt vault))
              (cv (/ (* coll p) PRICE_SCALE))
              (maxDebt (/ (* cv BPS) mincr)))
          (if (> maxDebt currDebt)
              (- maxDebt currDebt)
              u0)))))
