SELECT 미병그룹,count(*) FROM [dbo].[미병점수] GROUP BY 미병그룹


SELECT * INTO #tmp FROM (
SELECT 대상자등록번호,visit,미병점수_총점,미병그룹 FROM [dbo].[미병점수] WHERE 미병그룹 !='error'
) a

SELECT * INTO #TMP2 FROM (
 SELECT 대상자등록번호,VISIT,삶의질점수,SF12_PCS,SF12_MCS,EQ5D_VALUES FROM [dbo].[삶의질점수]
 )  A


 SELECT * FROM #TMP
 SELECT * FROM #TMP2
 d
 SELECT * INTO 미병_삶의질점수JOIN FROM(
 SELECT A.*,B.[삶의질점수],B.SF12_PCS,B.SF12_MCS,B.EQ5D_VALUES FROM #tmp A INNER JOIN #TMP2 B ON A.[대상자등록번호]=B.[대상자등록번호] AND A.visit=B.VISIT
 )V

 select * from 미병_삶의질점수JOIN a INNER JOIN 미병_삶의질점수JOIN b ON a.[대상자등록번호]=b.[대상자등록번호]
 WHERE a.visit='D1' AND b.visit='D3'

 SELECT a.[삶의질점수],a.SF12_PCS,a.SF12_MCS,a.EQ5D_VALUES,b.[삶의질점수],b.SF12_PCS,b.SF12_MCS,b.EQ5D_VALUES
 ,c.[삶의질점수],c.SF12_PCS,c.SF12_MCS,c.EQ5D_VALUES
 FROM ( select * from 미병_삶의질점수JOIN WHERE visit='D1') a INNER JOIN 
 (select * from 미병_삶의질점수JOIN WHERE visit='D3') b ON a.[대상자등록번호] =b.[대상자등록번호]	INNER JOIN 
  ( select * from 미병_삶의질점수JOIN WHERE visit='D8') c on a.[대상자등록번호] =c.[대상자등록번호]	



  
 SELECT a.[삶의질점수] as 삶의질점수1,a.SF12_PCS as SF12_PCS1,a.SF12_MCS as SF12_MCS1,a.EQ5D_VALUES as EQ5D_VALUES1,
 b.[삶의질점수] as 삶의질점수3,b.SF12_PCS  as SF12_PCS3,b.SF12_MCS as SF12_MC3,b.EQ5D_VALUES as EQ5D_VALUES3
  ,c.[삶의질점수] as 삶의질점수8,c.SF12_PCS as SF12_PC8,c.SF12_MCS SF12_MCS8,c.EQ5D_VALUES as EQ5D_VALUES8
 FROM ( select * from mydata4 WHERE visit='D1') a INNER JOIN 
 (select * from mydata4 WHERE visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]	INNER JOIN 
  ( select * from mydata4 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]	




	