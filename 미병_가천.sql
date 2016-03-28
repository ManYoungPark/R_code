select * from [dbo].[미병_V1#0$]
select * from [dbo].[삶의질$]
select * from [dbo].[계측정보$]

drop table 반복측정step1 
select * into 반복측정step1 from (
select a.*,삶의질점수,SF12_MCS,SF12_PCS,EQ5D_Value
 ,[허리둘레]
      ,[엉덩이둘레]
      ,[수축기혈압]
      ,[이완기혈압]
      ,[맥박]
      ,[체온_고막]
      ,[호흡]
      ,[체표_이마1]
      ,[체표_이마2]
      ,[체표_손바닥_좌1]
      ,[체표_손바닥_좌2]
      ,[체표_손바닥_우1]
      ,[체표_손바닥_우2]
 from [미병_V1#0$] a inner join [삶의질$] b on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit and a.Seq=b.Seq
 inner join [dbo].[계측정보$] c on a.대상자_등록번호=c.대상자_등록번호 and a.Visit=c.Visit and a.Seq=c.Seq
 )v
 
 
select New미병그룹,count(*) from 반복측정step1
group by New미병그룹

 DROP TABLE 반복측정_삶의질
 select * into 반복측정_삶의질 from (
 select a.대상자_등록번호,a.New미병그룹,a.삶의질점수 as 삶의질점수seq1,b.삶의질점수 as 삶의질점수seq2,c.삶의질점수 as 삶의질점수seq3 from 
 (select * from 반복측정step1 where seq=1) a inner join 
 (select * from 반복측정step1 where seq=2) b on a.대상자_등록번호=b.대상자_등록번호 inner join
 (select * from 반복측정step1 where seq=3) c on a.대상자_등록번호=c.대상자_등록번호
 )v



 select * from 반복측정step1
 delete from 반복측정step1 where new미병그룹=2
 
 drop table 가천미병_체성분
 select * into 가천미병_체성분 from (
 select b.*, a.New미병그룹 from 반복측정step1 a inner join [dbo].[체성분$] b on a.대상자_등록번호=b.대상자_등록번호 and a.Seq=b.Seq
 )V

 delete from 가천미병_체성분 where new미병그룹 is null
 select count(*),new미병그룹 from 가천미병_체성분
 group by new미병그룹

select New미병그룹,count(*) from 반복측정_삶의질
group by New미병그룹

DELETE FROM 반복측정_삶의질  WHERE New미병그룹=2


select * from [dbo].[반복측정step1]


select * from [dbo].[체성분$]


 USE [MB_가천]
GO

SELECT [대상자_등록번호]
      ,[Visit]
      ,[Seq]
      ,[미병점수_총점]
      ,[미병그룹]
      ,[New미병그룹]
      ,[삶의질점수]
      ,[SF12_MCS]
      ,[SF12_PCS]
      ,[EQ5D_Value]
  FROM [dbo].[반복측정step1]
GO

