SELECT �̺��׷�,count(*) FROM [dbo].[�̺�����] GROUP BY �̺��׷�


SELECT * INTO #tmp FROM (
SELECT ����ڵ�Ϲ�ȣ,visit,�̺�����_����,�̺��׷� FROM [dbo].[�̺�����] WHERE �̺��׷� !='error'
) a

SELECT * INTO #TMP2 FROM (
 SELECT ����ڵ�Ϲ�ȣ,VISIT,����������,SF12_PCS,SF12_MCS,EQ5D_VALUES FROM [dbo].[����������]
 )  A


 SELECT * FROM #TMP
 SELECT * FROM #TMP2
 d
 SELECT * INTO �̺�_����������JOIN FROM(
 SELECT A.*,B.[����������],B.SF12_PCS,B.SF12_MCS,B.EQ5D_VALUES FROM #tmp A INNER JOIN #TMP2 B ON A.[����ڵ�Ϲ�ȣ]=B.[����ڵ�Ϲ�ȣ] AND A.visit=B.VISIT
 )V

 select * from �̺�_����������JOIN a INNER JOIN �̺�_����������JOIN b ON a.[����ڵ�Ϲ�ȣ]=b.[����ڵ�Ϲ�ȣ]
 WHERE a.visit='D1' AND b.visit='D3'

 SELECT a.[����������],a.SF12_PCS,a.SF12_MCS,a.EQ5D_VALUES,b.[����������],b.SF12_PCS,b.SF12_MCS,b.EQ5D_VALUES
 ,c.[����������],c.SF12_PCS,c.SF12_MCS,c.EQ5D_VALUES
 FROM ( select * from �̺�_����������JOIN WHERE visit='D1') a INNER JOIN 
 (select * from �̺�_����������JOIN WHERE visit='D3') b ON a.[����ڵ�Ϲ�ȣ] =b.[����ڵ�Ϲ�ȣ]	INNER JOIN 
  ( select * from �̺�_����������JOIN WHERE visit='D8') c on a.[����ڵ�Ϲ�ȣ] =c.[����ڵ�Ϲ�ȣ]	



  
 SELECT a.[����������] as ����������1,a.SF12_PCS as SF12_PCS1,a.SF12_MCS as SF12_MCS1,a.EQ5D_VALUES as EQ5D_VALUES1,
 b.[����������] as ����������3,b.SF12_PCS  as SF12_PCS3,b.SF12_MCS as SF12_MC3,b.EQ5D_VALUES as EQ5D_VALUES3
  ,c.[����������] as ����������8,c.SF12_PCS as SF12_PC8,c.SF12_MCS SF12_MCS8,c.EQ5D_VALUES as EQ5D_VALUES8
 FROM ( select * from mydata4 WHERE visit='D1') a INNER JOIN 
 (select * from mydata4 WHERE visit='D3') b ON a.[�����.��Ϲ�ȣ] =b.[�����.��Ϲ�ȣ]	INNER JOIN 
  ( select * from mydata4 WHERE visit='D8') c on a.[�����.��Ϲ�ȣ] =c.[�����.��Ϲ�ȣ]	




	