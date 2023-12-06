
;; title: Friend.Tech Clone
;; version: 1.0
;; description: Friends.Tech Clone with fee management, access control, get buy, get sell, set percent & more! 


(define-map keysBalance { subject: principal, holder: principal } uint)
(define-map keysSupply { subject: principal } uint)


(define-read-only (get-price (supply uint) (amount uint))
  (let
    (
      (base-price u10)
      (price-change-factor u100)
      (adjusted-supply (+ supply amount))
    )
    (+ base-price (* amount (/ (* adjusted-supply adjusted-supply) price-change-factor)))
  )
)

(define-data-var protocolFeePercent uint u124) ;; Fee percentage (in basis points, for example)
(define-data-var protocolFeeDestination principal tx-sender) ;; Destination for collected fees




(define-public (buy-keys (subject principal) (amount uint))
  (let
    (
      (supply (default-to u0 (map-get? keysSupply { subject: subject })))
      (price (get-price supply amount))
      ;; Calculate fee
      (fee (if (is-eq supply u0)
              u0 ;; No fee for initial purchase
              (/ (* price (var-get protocolFeePercent)) u10000))) ;; Fee calculation
      ;; Calculate total price including fee
      (total-price (+ price fee))
    )
    ;; Check if the user has enough funds to cover the total price
    (if (>= (stx-get-balance tx-sender) total-price)
      (begin
        ;; Transfer fee only if it's greater than zero
        (if (> fee u0)
          (match (stx-transfer? fee tx-sender (var-get protocolFeeDestination))
            fee-transfer-success
            ;; On successful fee transfer, proceed with the price transfer
            (match (stx-transfer? price tx-sender (as-contract tx-sender))
              price-transfer-success
              ;; On successful price transfer, update keys balance and supply
              (begin
                (map-set keysBalance { subject: subject, holder: tx-sender }
                         (+ (default-to u0 (map-get? keysBalance { subject: subject, holder: tx-sender })) amount))
                (map-set keysSupply { subject: subject } (+ supply amount))
                (ok true)
              )
              ;; Handle error in price transfer
              price-transfer-error (err u4)
            )
            ;; Handle error in fee transfer
            fee-transfer-error (err u3)
          )
          ;; If no fee, directly transfer the price
          (match (stx-transfer? price tx-sender (as-contract tx-sender))
            price-transfer-success
            ;; On successful price transfer, update keys balance and supply
            (begin
              (map-set keysBalance { subject: subject, holder: tx-sender }
                       (+ (default-to u0 (map-get? keysBalance { subject: subject, holder: tx-sender })) amount))
              (map-set keysSupply { subject: subject } (+ supply amount))
              (ok true)
            )
            ;; Handle error in price transfer
            price-transfer-error (err u4)
          )
        )
      )
      ;; Error if insufficient funds
      (err u2)
    )
  )
)



(define-public (sell-keys (subject principal) (amount uint))
  (let
    (
      (supply (default-to u0 (map-get? keysSupply { subject: subject })))
      (holder-balance (default-to u0 (map-get? keysBalance { subject: subject, holder: tx-sender })))
      (price (get-price supply amount))
      (fee (/ (* price (var-get protocolFeePercent)) u10000))
      (total-return (- price fee))
      ;; Check if the holder has enough keys to sell
      (valid-sell (>= holder-balance amount))
    )
    (if valid-sell
      (begin
        ;; Transfer fee to protocolFeeDestination
        (match (stx-transfer? fee tx-sender (var-get protocolFeeDestination))
          fee-transfer-success
          ;; On successful fee transfer, proceed to transfer the total return to user
          (match (stx-transfer? total-return (as-contract tx-sender) tx-sender)
            return-transfer-success
            ;; On successful return transfer, update keys balance and supply
            ;; [Your logic for updating balances and supply]
            (ok true)
            return-transfer-error (err u3)
          )
          fee-transfer-error (err u2)
        )
      )
      (err u1) ;; Error if the sell is not valid (e.g., insufficient balance)
    )
  )
)


(define-read-only (is-keyholder (subject principal) (holder principal))
  (>= (default-to u0 (map-get? keysBalance { subject: subject, holder: holder })) u1)
)

(define-read-only (get-keys-balance (subject principal) (holder principal))
  (match (map-get? keysBalance {subject: subject, holder: holder})
    entry (ok entry) ;; directly return the entry if found
    (ok u0)) ;; return 0 if not found
)

(define-read-only (get-keys-supply (subject principal))
  (match (map-get? keysSupply {subject: subject})
    entry (ok entry) ;; directly return the entry if found
    (ok u0)) ;; return 0 if not found
)

(define-read-only (get-buy-price (subject principal) (amount uint))
  ;; Implement buy price logic
  (let 
    ((supply (default-to u0 (map-get? keysSupply { subject: subject })))
     (base-price u10) ;; assuming a base price, adjust as necessary
     (price-change-factor u100) ;; assuming a price change factor, adjust as necessary
    )
    (+ base-price (* amount (/ (* (+ supply amount) (+ supply amount)) price-change-factor)))
  )
)

(define-read-only (get-sell-price (subject principal) (amount uint))
  ;; Implement sell price logic
  (let 
    ((supply (default-to u0 (map-get? keysSupply { subject: subject })))
     (base-price u10) ;; assuming a base price, adjust as necessary
     (price-change-factor u100) ;; assuming a price change factor, adjust as necessary
    )
    ;; You might want to adjust the formula for selling, depending on your pricing model
    (+ base-price (* amount (/ (* (- supply amount) (- supply amount)) price-change-factor)))
  )
)


(define-data-var contractOwner principal tx-sender)

(define-public (set-protocol-fee-percent (feePercent uint))
  ;; Check if the caller is the contractOwner
  (if (is-eq tx-sender (var-get contractOwner))
    (begin
      ;; Update the protocolFeePercent value
      (var-set protocolFeePercent feePercent)
      (ok "Fee percent updated successfully.")
    )
    (err "Unauthorized: Caller is not the contract owner.")
  )
)

(define-read-only (protocol-fee-percent) 
    (ok (var-get protocolFeePercent) )
)
