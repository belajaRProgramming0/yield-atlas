-- Synthetic closed transactions, not cash flow or actual disbursements.
WITH monthly AS (
  SELECT substr(closed_date, 1, 7) AS month,
         COUNT(*) AS closed_transactions, SUM(redeemed) AS redeemed_transactions
  FROM pawn_history GROUP BY substr(closed_date, 1, 7)
)
SELECT *, LAG(closed_transactions) OVER (ORDER BY month) AS previous_month_transactions,
       closed_transactions - LAG(closed_transactions) OVER (ORDER BY month) AS monthly_change
FROM monthly ORDER BY month;
