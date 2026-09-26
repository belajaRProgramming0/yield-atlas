-- Outcome labels intentionally excluded. Aggregate before joining to avoid fan-out.
WITH history AS (
  SELECT a.application_id, COUNT(p.pawn_id) AS pawn_trans_count_2yr,
         AVG(CAST(p.redeemed AS REAL)) AS pawn_repayment_rate
  FROM applications a
  LEFT JOIN pawn_history p ON a.customer_id = p.customer_id
    AND p.closed_date < a.application_date
    AND p.closed_date >= date(a.application_date, '-730 days')
  GROUP BY a.application_id
)
SELECT a.*, c.annual_income, c.employment_years, c.prior_default, c.source,
       h.pawn_trans_count_2yr, h.pawn_repayment_rate
FROM applications a
JOIN customers c ON c.customer_id = a.customer_id
JOIN history h ON h.application_id = a.application_id
ORDER BY a.application_id;
