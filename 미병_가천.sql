select * from [dbo].[�̺�_V1#0$]
select * from [dbo].[������$]
select * from [dbo].[��������$]

drop table �ݺ�����step1 
select * into �ݺ�����step1 from (
select a.*,����������,SF12_MCS,SF12_PCS,EQ5D_Value
 ,[�㸮�ѷ�]
      ,[�����̵ѷ�]
      ,[���������]
      ,[�̿ϱ�����]
      ,[�ƹ�]
      ,[ü��_��]
      ,[ȣ��]
      ,[üǥ_�̸�1]
      ,[üǥ_�̸�2]
      ,[üǥ_�չٴ�_��1]
      ,[üǥ_�չٴ�_��2]
      ,[üǥ_�չٴ�_��1]
      ,[üǥ_�չٴ�_��2]
 from [�̺�_V1#0$] a inner join [������$] b on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit and a.Seq=b.Seq
 inner join [dbo].[��������$] c on a.�����_��Ϲ�ȣ=c.�����_��Ϲ�ȣ and a.Visit=c.Visit and a.Seq=c.Seq
 )v
 
 
select New�̺��׷�,count(*) from �ݺ�����step1
group by New�̺��׷�

 DROP TABLE �ݺ�����_������
 select * into �ݺ�����_������ from (
 select a.�����_��Ϲ�ȣ,a.New�̺��׷�,a.���������� as ����������seq1,b.���������� as ����������seq2,c.���������� as ����������seq3 from 
 (select * from �ݺ�����step1 where seq=1) a inner join 
 (select * from �ݺ�����step1 where seq=2) b on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ inner join
 (select * from �ݺ�����step1 where seq=3) c on a.�����_��Ϲ�ȣ=c.�����_��Ϲ�ȣ
 )v



 select * from �ݺ�����step1
 delete from �ݺ�����step1 where new�̺��׷�=2
 
 drop table ��õ�̺�_ü����
 select * into ��õ�̺�_ü���� from (
 select b.*, a.New�̺��׷� from �ݺ�����step1 a inner join [dbo].[ü����$] b on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Seq=b.Seq
 )V

 delete from ��õ�̺�_ü���� where new�̺��׷� is null
 select count(*),new�̺��׷� from ��õ�̺�_ü����
 group by new�̺��׷�

select New�̺��׷�,count(*) from �ݺ�����_������
group by New�̺��׷�

DELETE FROM �ݺ�����_������  WHERE New�̺��׷�=2


select * from [dbo].[�ݺ�����step1]


select * from [dbo].[ü����$]


 USE [MB_��õ]
GO

SELECT [�����_��Ϲ�ȣ]
      ,[Visit]
      ,[Seq]
      ,[�̺�����_����]
      ,[�̺��׷�]
      ,[New�̺��׷�]
      ,[����������]
      ,[SF12_MCS]
      ,[SF12_PCS]
      ,[EQ5D_Value]
  FROM [dbo].[�ݺ�����step1]
GO

