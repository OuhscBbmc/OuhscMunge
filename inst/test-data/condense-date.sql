--Use cdw_cache_staging
DROP TABLE ou_physicians_quality_measurement.tbl_condense_date


SELECT 
  TOP (1000) 
  s.pid
  ,s.sdid
  ,s.cvx_family
  , s.consecution 
  ,MIN(s.match_first_in_sdid_cvx_family_consecution)   AS cvx_out

  ,MIN(s.obsdate)               AS obs_date
  ,MIN(s.obsvalue)              AS obs_value
  ,COUNT(DISTINCT(s.obsid))     AS obs_id_count
  ,COUNT(DISTINCT(s.obsdate))   AS obs_date_count
  ,MIN(s.priority)              AS priority_min
INTO ou_physicians_quality_measurement.tbl_condense_date

FROM ou_physicians_quality_measurement.tbl_match_date_step_1 AS s
--where sdid = 1651756984013180
GROUP BY s.pid, s.SDID, s.cvx_family, s.consecution 
