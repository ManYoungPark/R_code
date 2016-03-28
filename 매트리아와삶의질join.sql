USE [metria]
GO

--day별 max 값

[dbo].[미병EQ5Dvisit]
drop table tmp_metria_summaryDay_PMY
select * into tmp_metria_summaryDay_PMY from (
SELECT ID   
      ,CURRENTDATE
      ,max(TRANSVERSE_ACCEL_PEAKS)  TRANSVERSE_ACCEL_PEAKS_max
      ,max(FORWARD_ACCEL_PEAKS) FORWARD_ACCEL_PEAKS_max
      ,max(LONGITUDINAL_ACCEL_PEAKS) LONGITUDINAL_ACCEL_PEAKS_max
      ,max(SKIN_TEMP_AVERAGE) SKIN_TEMP_AVERAGE_max
      ,max(TRANSVERSE_ACCEL_AVERAGE) TRANSVERSE_ACCEL_AVERAGE_max
      ,max(LONGITUDINAL_ACCEL_AVERAGE) LONGITUDINAL_ACCEL_AVERAGE_max
      ,max(NEAR_BODY_TEMP_AVERAGE) NEAR_BODY_TEMP_AVERAGE_max
      ,max(TRANSVERSE_ACCEL_MAD)  TRANSVERSE_ACCEL_MAD_max
      ,max(LONGITUDINAL_ACCEL_MAD)  LONGITUDINAL_ACCEL_MAD_max
      ,max(STEP_COUNTER)  STEP_COUNTER_max
      ,max(FORWARD_ACCEL_AVERAGE)  FORWARD_ACCEL_AVERAGE_max
      ,max(FORWARD_ACCEL_MAD)  FORWARD_ACCEL_MAD_max
      ,max(GSR_AVERAGE)  GSR_AVERAGE_max
      ,sum(LYING_DOWN)  LYING_DOWN_sum
      ,sum(SLEEP)  SLEEP_sum
      ,sum(PHYSICAL_ACTIVITY)  PHYSICAL_ACTIVITY_sum
      ,max(ENERGY_EXPENDITURE)  ENERGY_EXPENDITURE_max
      ,sum(SEDENTARY)  SEDENTARY_sum
      ,sum(LIGHT)  LIGHT_sum
      ,sum(MODERATE)  MODERATE_sum
      ,sum(VIGOROUS)  VIGOROUS_sum
      ,sum(VERY_VIGOROUS)  VERY_VIGOROUS_sum
      ,max(METS)  METS_max
      ,max(SPEED)  SPEED_max
      ,max(DISTANCE)  DISTANCE_max
      ,sum(cast(ACTIVITY_CLASSIFICATIONS as int))  ACTIVITY_CLASSIFICATIONS_sum
      ,sum(cast(SLEEP_CLASSIFICATIONS as int))  SLEEP_CLASSIFICATIONS_sum
      ,max(HEAT_FLUX_AVERAGE)  HEAT_FLUX_AVERAGE_max
  FROM metria_data
  group by id,currentdate
  )v


  
  select * into tmp_metria_summaryWithIn3Day_PMY from (
  select id,max(TRANSVERSE_ACCEL_PEAKS_max)  TRANSVERSE_ACCEL_PEAKS_max
      ,max(FORWARD_ACCEL_PEAKS_max) FORWARD_ACCEL_PEAKS_max
      ,max(LONGITUDINAL_ACCEL_PEAKS_max) LONGITUDINAL_ACCEL_PEAKS_max
      ,max(SKIN_TEMP_AVERAGE_max) SKIN_TEMP_AVERAGE_max
      ,max(TRANSVERSE_ACCEL_AVERAGE_max) TRANSVERSE_ACCEL_AVERAGE_max
      ,max(LONGITUDINAL_ACCEL_AVERAGE_max) LONGITUDINAL_ACCEL_AVERAGE_max
      ,max(NEAR_BODY_TEMP_AVERAGE_max) NEAR_BODY_TEMP_AVERAGE_max
      ,max(TRANSVERSE_ACCEL_MAD_max)  TRANSVERSE_ACCEL_MAD_max
      ,max(LONGITUDINAL_ACCEL_MAD_max)  LONGITUDINAL_ACCEL_MAD_max
      ,max(STEP_COUNTER_max)  STEP_COUNTER_max
      ,max(FORWARD_ACCEL_AVERAGE_max)  FORWARD_ACCEL_AVERAGE_max
      ,max(FORWARD_ACCEL_MAD_max)  FORWARD_ACCEL_MAD_max
      ,max(GSR_AVERAGE_max)  GSR_AVERAGE_max
      ,sum(LYING_DOWN_sum)  LYING_DOWN_sum
      ,sum(SLEEP_sum)  SLEEP_sum
      ,sum(PHYSICAL_ACTIVITY_sum)  PHYSICAL_ACTIVITY_sum
      ,max(ENERGY_EXPENDITURE_max)  ENERGY_EXPENDITURE_max
      ,sum(SEDENTARY_sum)  SEDENTARY_sum
      ,sum(LIGHT_sum)  LIGHT_sum
      ,sum(MODERATE_sum)  MODERATE_sum
      ,sum(VIGOROUS_sum)  VIGOROUS_sum
      ,sum(VERY_VIGOROUS_sum)  VERY_VIGOROUS_sum
      ,max(METS_max)  METS_max
      ,max(SPEED_max)  SPEED_max
      ,max(DISTANCE_max)  DISTANCE_max
      ,sum(ACTIVITY_CLASSIFICATIONS_sum )  ACTIVITY_CLASSIFICATIONS_sum
      ,sum(SLEEP_CLASSIFICATIONS_sum)  SLEEP_CLASSIFICATIONS_sum
      ,max(HEAT_FLUX_AVERAGE_max)  HEAT_FLUX_AVERAGE_max from tmp_metria_summaryDay_PMY
  where currentdate<4
  group by id
  )v
  
select * into tmp_metria_summaryWithIn3Day_PMY_점수 from (
  select a.*,삶의질점수,sf12_pcs,sf12_mcs,eq5d_values from tmp_metria_summaryWithIn3Day_PMY a inner join [dbo].[미병EQ5Dvisit] b on a.id=b.[대상자등록번호]
  )v


  select * into tmp_metria_summaryWithIn3_8Day_PMY from (
  select id,max(TRANSVERSE_ACCEL_PEAKS_max)  TRANSVERSE_ACCEL_PEAKS_max
      ,max(FORWARD_ACCEL_PEAKS_max) FORWARD_ACCEL_PEAKS_max
      ,max(LONGITUDINAL_ACCEL_PEAKS_max) LONGITUDINAL_ACCEL_PEAKS_max
      ,max(SKIN_TEMP_AVERAGE_max) SKIN_TEMP_AVERAGE_max
      ,max(TRANSVERSE_ACCEL_AVERAGE_max) TRANSVERSE_ACCEL_AVERAGE_max
      ,max(LONGITUDINAL_ACCEL_AVERAGE_max) LONGITUDINAL_ACCEL_AVERAGE_max
      ,max(NEAR_BODY_TEMP_AVERAGE_max) NEAR_BODY_TEMP_AVERAGE_max
      ,max(TRANSVERSE_ACCEL_MAD_max)  TRANSVERSE_ACCEL_MAD_max
      ,max(LONGITUDINAL_ACCEL_MAD_max)  LONGITUDINAL_ACCEL_MAD_max
      ,max(STEP_COUNTER_max)  STEP_COUNTER_max
      ,max(FORWARD_ACCEL_AVERAGE_max)  FORWARD_ACCEL_AVERAGE_max
      ,max(FORWARD_ACCEL_MAD_max)  FORWARD_ACCEL_MAD_max
      ,max(GSR_AVERAGE_max)  GSR_AVERAGE_max
      ,sum(LYING_DOWN_sum)  LYING_DOWN_sum
      ,sum(SLEEP_sum)  SLEEP_sum
      ,sum(PHYSICAL_ACTIVITY_sum)  PHYSICAL_ACTIVITY_sum
      ,max(ENERGY_EXPENDITURE_max)  ENERGY_EXPENDITURE_max
      ,sum(SEDENTARY_sum)  SEDENTARY_sum
      ,sum(LIGHT_sum)  LIGHT_sum
      ,sum(MODERATE_sum)  MODERATE_sum
      ,sum(VIGOROUS_sum)  VIGOROUS_sum
      ,sum(VERY_VIGOROUS_sum)  VERY_VIGOROUS_sum
      ,max(METS_max)  METS_max
      ,max(SPEED_max)  SPEED_max
      ,max(DISTANCE_max)  DISTANCE_max
      ,sum(ACTIVITY_CLASSIFICATIONS_sum )  ACTIVITY_CLASSIFICATIONS_sum
      ,sum(SLEEP_CLASSIFICATIONS_sum)  SLEEP_CLASSIFICATIONS_sum
      ,max(HEAT_FLUX_AVERAGE_max)  HEAT_FLUX_AVERAGE_max from tmp_metria_summaryDay_PMY
  where currentdate>=4
  group by id
  )v
  
select * into tmp_metria_summaryWithIn3_8Day_PMY_점수 from (
  select a.*,삶의질점수,sf12_pcs,sf12_mcs,eq5d_values from tmp_metria_summaryWithIn3_8Day_PMY a inner join [dbo].[미병EQ5DvisitD8] b on a.id=b.[대상자등록번호]
  )v











select sum(lying_down) from [dbo].[metria_data] where id='KM1-G02-H0024'
and currentdate=2

select * from tmp_metria_summaryDayAvg_PMY where id='KM1-G02-H0024'
  --avg 값

--day별 avg 값

[dbo].[미병EQ5Dvisit]
drop table tmp_metria_summaryDayAvg_PMY
select * into tmp_metria_summaryDayAvg_PMY from (
SELECT ID   
      ,CURRENTDATE
      ,avg(TRANSVERSE_ACCEL_PEAKS)  TRANSVERSE_ACCEL_PEAKS_avg
      ,avg(FORWARD_ACCEL_PEAKS) FORWARD_ACCEL_PEAKS_avg
      ,avg(LONGITUDINAL_ACCEL_PEAKS) LONGITUDINAL_ACCEL_PEAKS_avg
      ,avg(SKIN_TEMP_AVERAGE) SKIN_TEMP_AVERAGE_avg
      ,avg(TRANSVERSE_ACCEL_AVERAGE) TRANSVERSE_ACCEL_AVERAGE_avg
      ,avg(LONGITUDINAL_ACCEL_AVERAGE) LONGITUDINAL_ACCEL_AVERAGE_avg
      ,avg(NEAR_BODY_TEMP_AVERAGE) NEAR_BODY_TEMP_AVERAGE_avg
      ,avg(TRANSVERSE_ACCEL_MAD)  TRANSVERSE_ACCEL_MAD_avg
      ,avg(LONGITUDINAL_ACCEL_MAD)  LONGITUDINAL_ACCEL_MAD_avg
      ,avg(STEP_COUNTER)  STEP_COUNTER_avg
      ,avg(FORWARD_ACCEL_AVERAGE)  FORWARD_ACCEL_AVERAGE_avg
      ,avg(FORWARD_ACCEL_MAD)  FORWARD_ACCEL_MAD_avg
      ,avg(GSR_AVERAGE)  GSR_AVERAGE_avg
      ,sum(LYING_DOWN)  LYING_DOWN_sum
      ,sum(SLEEP)  SLEEP_sum
      ,sum(PHYSICAL_ACTIVITY)  PHYSICAL_ACTIVITY_sum
      ,avg(ENERGY_EXPENDITURE)  ENERGY_EXPENDITURE_avg
      ,sum(SEDENTARY)  SEDENTARY_sum
      ,sum(LIGHT)  LIGHT_sum
      ,sum(MODERATE)  MODERATE_sum
      ,sum(VIGOROUS)  VIGOROUS_sum
      ,sum(VERY_VIGOROUS)  VERY_VIGOROUS_sum
      ,avg(METS)  METS_avg
      ,avg(SPEED)  SPEED_avg
      ,avg(DISTANCE)  DISTANCE_avg
      ,sum(cast(ACTIVITY_CLASSIFICATIONS as int))  ACTIVITY_CLASSIFICATIONS_sum
      ,sum(cast(SLEEP_CLASSIFICATIONS as int))  SLEEP_CLASSIFICATIONS_sum
      ,avg(HEAT_FLUX_AVERAGE)  HEAT_FLUX_AVERAGE_avg
  FROM metria_data
  group by id,currentdate
  )v

  select top 100 * from tmp_metria_summaryDayAvg_PMY where KM1-G02-H0024
  
  select * into tmp_metria_summary_Avg_WithIn3Day_PMY from (
  select id,avg(TRANSVERSE_ACCEL_PEAKS_avg)  TRANSVERSE_ACCEL_PEAKS_avg
      ,avg(FORWARD_ACCEL_PEAKS_avg) FORWARD_ACCEL_PEAKS_avg
      ,avg(LONGITUDINAL_ACCEL_PEAKS_avg) LONGITUDINAL_ACCEL_PEAKS_avg
      ,avg(SKIN_TEMP_AVERAGE_avg) SKIN_TEMP_AVERAGE_avg
      ,avg(TRANSVERSE_ACCEL_AVERAGE_avg) TRANSVERSE_ACCEL_AVERAGE_avg
      ,avg(LONGITUDINAL_ACCEL_AVERAGE_avg) LONGITUDINAL_ACCEL_AVERAGE_avg
      ,avg(NEAR_BODY_TEMP_AVERAGE_avg) NEAR_BODY_TEMP_AVERAGE_avg
      ,avg(TRANSVERSE_ACCEL_MAD_avg)  TRANSVERSE_ACCEL_MAD_avg
      ,avg(LONGITUDINAL_ACCEL_MAD_avg)  LONGITUDINAL_ACCEL_MAD_avg
      ,avg(STEP_COUNTER_avg)  STEP_COUNTER_avg
      ,avg(FORWARD_ACCEL_AVERAGE_avg)  FORWARD_ACCEL_AVERAGE_avg
      ,avg(FORWARD_ACCEL_MAD_avg)  FORWARD_ACCEL_MAD_avg
      ,avg(GSR_AVERAGE_avg)  GSR_AVERAGE_avg
      ,sum(LYING_DOWN_sum)  LYING_DOWN_sum
      ,sum(SLEEP_sum)  SLEEP_sum
      ,sum(PHYSICAL_ACTIVITY_sum)  PHYSICAL_ACTIVITY_sum
      ,avg(ENERGY_EXPENDITURE_avg)  ENERGY_EXPENDITURE_avg
      ,sum(SEDENTARY_sum)  SEDENTARY_sum
      ,sum(LIGHT_sum)  LIGHT_sum
      ,sum(MODERATE_sum)  MODERATE_sum
      ,sum(VIGOROUS_sum)  VIGOROUS_sum
      ,sum(VERY_VIGOROUS_sum)  VERY_VIGOROUS_sum
      ,avg(METS_avg)  METS_avg
      ,avg(SPEED_avg)  SPEED_avg
      ,avg(DISTANCE_avg)  DISTANCE_avg
      ,sum(ACTIVITY_CLASSIFICATIONS_sum )  ACTIVITY_CLASSIFICATIONS_sum
      ,sum(SLEEP_CLASSIFICATIONS_sum)  SLEEP_CLASSIFICATIONS_sum
      ,avg(HEAT_FLUX_AVERAGE_avg)  HEAT_FLUX_AVERAGE_avg from tmp_metria_summaryDayAvg_PMY
  where currentdate<4
  group by id
  )v
  


select * into tmp_metria_summaryWithIn3Day_PMY_점수 from (
  select a.*,삶의질점수,sf12_pcs,sf12_mcs,eq5d_values from tmp_metria_summaryWithIn3Day_PMY a inner join [dbo].[미병EQ5Dvisit] b on a.id=b.[대상자등록번호]
  )v


  select * into tmp_metria_summary_Avg_WithIn3_8Day_PMY from (
  select id,avg(TRANSVERSE_ACCEL_PEAKS_avg)  TRANSVERSE_ACCEL_PEAKS_avg
      ,avg(FORWARD_ACCEL_PEAKS_avg) FORWARD_ACCEL_PEAKS_avg
      ,avg(LONGITUDINAL_ACCEL_PEAKS_avg) LONGITUDINAL_ACCEL_PEAKS_avg
      ,avg(SKIN_TEMP_AVERAGE_avg) SKIN_TEMP_AVERAGE_avg
      ,avg(TRANSVERSE_ACCEL_AVERAGE_avg) TRANSVERSE_ACCEL_AVERAGE_avg
      ,avg(LONGITUDINAL_ACCEL_AVERAGE_avg) LONGITUDINAL_ACCEL_AVERAGE_avg
      ,avg(NEAR_BODY_TEMP_AVERAGE_avg) NEAR_BODY_TEMP_AVERAGE_avg
      ,avg(TRANSVERSE_ACCEL_MAD_avg)  TRANSVERSE_ACCEL_MAD_avg
      ,avg(LONGITUDINAL_ACCEL_MAD_avg)  LONGITUDINAL_ACCEL_MAD_avg
      ,avg(STEP_COUNTER_avg)  STEP_COUNTER_avg
      ,avg(FORWARD_ACCEL_AVERAGE_avg)  FORWARD_ACCEL_AVERAGE_avg
      ,avg(FORWARD_ACCEL_MAD_avg)  FORWARD_ACCEL_MAD_avg
      ,avg(GSR_AVERAGE_avg)  GSR_AVERAGE_avg
      ,sum(LYING_DOWN_sum)  LYING_DOWN_sum
      ,sum(SLEEP_sum)  SLEEP_sum
      ,sum(PHYSICAL_ACTIVITY_sum)  PHYSICAL_ACTIVITY_sum
      ,avg(ENERGY_EXPENDITURE_avg)  ENERGY_EXPENDITURE_avg
      ,sum(SEDENTARY_sum)  SEDENTARY_sum
      ,sum(LIGHT_sum)  LIGHT_sum
      ,sum(MODERATE_sum)  MODERATE_sum
      ,sum(VIGOROUS_sum)  VIGOROUS_sum
      ,sum(VERY_VIGOROUS_sum)  VERY_VIGOROUS_sum
      ,avg(METS_avg)  METS_avg
      ,avg(SPEED_avg)  SPEED_avg
      ,avg(DISTANCE_avg)  DISTANCE_avg
      ,sum(ACTIVITY_CLASSIFICATIONS_sum )  ACTIVITY_CLASSIFICATIONS_sum
      ,sum(SLEEP_CLASSIFICATIONS_sum)  SLEEP_CLASSIFICATIONS_sum
      ,avg(HEAT_FLUX_AVERAGE_avg)  HEAT_FLUX_AVERAGE_avg from tmp_metria_summaryDayAvg_PMY
  where currentdate>=4
  group by id
  )v
  


  select * from tmp_metria_summary_Avg_WithIn3Day_PMY
  alter table tmp_metria_summary_Avg_WithIn3Day_PMY add  fg char(20)

  update tmp_metria_summary_Avg_WithIn3Day_PMY set fg='1' 
  where [id] in ('KM1-G02-H0033'
,'KM1-G08-H0004'
,'KM1-G08-H0026'
,'KM1-G08-H0041'
,'KM1-G02-H0028'
,'KM1-G02-H0018'
,'KM1-G08-H0035'
,'KM1-G08-H0046'
,'KM1-G08-H0008'
,'KM1-G08-H0037'
,'KM1-G08-H0018'
,'KM1-G08-H0048'
,'KM1-G08-H0006'
,'KM1-G08-H0014'
,'KM1-G08-H0030'
,'KM1-G08-H0005'
,'KM1-G08-H0010'
,'KM1-G08-H0012'
,'KM1-G08-H0019'
,'KM1-G08-H0038'
,'KM1-G08-H0034'
,'KM1-G02-H0032'
,'KM1-G08-H0023'
,'KM1-G02-H0031'
,'KM1-G08-H0027')


  update tmp_metria_summary_Avg_WithIn3Day_PMY set fg='2' 
  where [id] in (
'KM1-G02-H0030'
,'KM1-G02-H0023'
,'KM1-G02-H0013'
,'KM1-G08-H0036'
,'KM1-G02-H0029'
,'KM1-G08-H0021'
,'KM1-G02-H0007'
,'KM1-G08-H0025'
,'KM1-G08-H0042'
,'KM1-G02-H0003'
,'KM1-G02-H0025')


select * from [dbo].[미병EQ5DvisitD8]



alter table tmp_metria_summary_Avg_WithIn3_8Day_PMY add  fg char(20)

  update tmp_metria_summary_Avg_WithIn3_8Day_PMY set fg='1' 
  where [id] in ('KM1-G02-H0033'
,'KM1-G08-H0004'
,'KM1-G08-H0026'
,'KM1-G08-H0041'
,'KM1-G02-H0028'
,'KM1-G02-H0018'
,'KM1-G08-H0035'
,'KM1-G08-H0046'
,'KM1-G08-H0008'
,'KM1-G08-H0037'
,'KM1-G08-H0018'
,'KM1-G08-H0048'
,'KM1-G08-H0006'
,'KM1-G08-H0014'
,'KM1-G08-H0030'
,'KM1-G08-H0005'
,'KM1-G08-H0010'
,'KM1-G08-H0012'
,'KM1-G08-H0019'
,'KM1-G08-H0038'
,'KM1-G08-H0034'
,'KM1-G02-H0032'
,'KM1-G08-H0023'
,'KM1-G02-H0031'
,'KM1-G08-H0027')


  update tmp_metria_summary_Avg_WithIn3_8Day_PMY set fg='2' 
  where [id] in (
'KM1-G02-H0030'
,'KM1-G02-H0023'
,'KM1-G02-H0013'
,'KM1-G08-H0036'
,'KM1-G02-H0029'
,'KM1-G08-H0021'
,'KM1-G02-H0007'
,'KM1-G08-H0025'
,'KM1-G08-H0042'
,'KM1-G02-H0003'
,'KM1-G02-H0025')




select top 100 * from tmp_metria_summary_Avg_WithIn3Day_PMY
delete from tmp_metria_summary_Avg_WithIn3Day_PMY where fg is null
delete from tmp_metria_summary_Avg_WithIn3_8Day_PMY where fg is null


