select * from [dbo].[MI_v2_가천경희_0324]

drop table mi7점척도10분위_2
select * into mi7점척도10분위_2 from (
select a.*,b.mi_total_score,b.mi_total_group1 as mi_total_group3  from [dbo].[인바디770통합_0308] a inner join [dbo].[mi7점척도10분위] b
on a.sub_id=b.확인id where v_d_s='1-0-0' and mi_total_group1 is not null
)v


select * from [dbo].[mi7점척도10분위_2]


select * from [dbo].[mi7점척도4분위]


--MI_V2는 가천,경희,대전 전부 있는 데이터임(ID와 미병분류)

drop table mi7점척도4분위대전가천경희_2
select * into mi7점척도4분위대전가천경희_2 from (
select a.*,b.mi_total_score,b.mi_total_group3 as mi_total_group3  from [dbo].[인바디770통합_0308] a inner join [dbo].[MI_v2$] b
on a.sub_id=b.확인id where v_d_s='1-0-0' and (mi_total_group3 is not null AND mi_total_group3!='')
)v


--중복 확인
SELECT mi_total_group3,COUNT(*) FROM mi7점척도4분위대전가천경희_2
GROUP BY mi_total_group3 HAVING COUNT(*)>1



--4월 6일..맥자료 이용
drop table mac_data
select * into mac_data from (
select 확인id,b.Energy,b.HR
,ESV
,ESI
,ECO
,ECO_per_
,ECI
,ECR
,ECRI
,MBP
,BP
,b.RAI

,MeanPeriod
,VarPeriod
,MeanMag
,VarMag
,H1
,H2
,H3
,H4
,H5
,T1
,T2
,T3
,T4
,T5
,T_T4
,T
,W
,Ap
,[As]
,Ad
,Ripple
,Aw
,Angle
,c.RAI as Mh_RAI

,RAI_div_HR
,BSA
,E_div_min
,EIX
,T1_div_T
,T2_div_T
,T3_div_T
,T4_div_T
,W_div_T
,T4_div__T_T4_
,Pf_div_Pl
,Ft_div_Sk
,Fs_div_Sl
,Sp_div_Cp
,Lo_div_Sh
,La_div_Fi
,d.Energy as Sh_Energy

,a
,b
,c
,d
,e
,b_div_a
,c_div_a
,d_div_a
,e_div_a

,mi_total_Group3

from [dbo].[MI_v2$] a inner join  [HDresult$]  b on a.확인id=(b.id+'-'+b.V_D_S)
inner join [dbo].[Mh$] c on a.확인id=(c.id+'-'+c.V_D_S)
inner join [dbo].[Sh$] d on a.확인id=(d.id+'-'+d.V_D_S)
inner join [dbo].[APG$] e on a.확인id=(e.id+'-'+e.V_D_S)
where mi_total_Group3 in (1,2,3) and b.energy is not null

)v
select * from [dbo].[MI_v2$] 
select * from mac_data
select 'a'+'b'


select * from [dbo].[HDresult$]


--데이터가 잘못되서 호석샘이 준 데이터로 다시 분석 2016-04-07 

drop table mi7점척도4분위대전가천경희_3
select * into mi7점척도4분위대전가천경희_3 from (
select a.*,b.mi_total_score,b.mi_total_group3 as mi_total_group3  from [dbo].[인바디770통합_0407_hs] a inner join [dbo].[MI_v2$] b
on a.sub_id=b.확인id where a.[v_d_s]='1-0-0' and (mi_total_group3 is not null AND mi_total_group3!='')

)v

select * from [dbo].[mi7점척도4분위대전가천경희_2]



--2106
select * from [dbo].[MI_v2_mapping] where 확인 like '%대전%'  --대전이라고 써있는것은 773 점 나머지는 777점



drop table 대전대만
select * into 대전대만 from (

select a.*,b.mi_total_score,b.mi_total_group3 as mi_total_group3  from [dbo].[인바디770통합_0407_hs] a inner join [dbo].[MI_v2_mapping] b
on a.sub_id=b.확인id where a.[v_d_s]='1-0-0' and (mi_total_group3 is not null AND mi_total_group3!='')
and b.확인 like '%대전%'

)v


select * from 대전대만

--2016-04-28 보고서 쓰기전에 .. 기현샘에게 받은 새로운 데이터들을 이용하여 분석
--설문지 version 2.1(227)과 v1(777)(건강한 사람들만, 증상없음에 체크한 대상자들임. ) 과 통합.. 

select * from [dbo].[MIv2#1과MIv1#0$]

sp_rename '분석변수$', '인바디분석변수160428'
sp_rename 'HRV분석자료$', 'HRV분석자료160428'
sp_rename '혈액분석자료$', '혈액분석자료160428'
sp_rename '맥분석자료s$', '맥분석자료160428'

select * from [MIv2#1과MIv1#0$]

select * into 인바디분석변수160428with미병분류 from (

select * from 인바디분석변수160428 a inner join [dbo].[MIv2#1과MIv1#0$] b

on a.[인바디 ID]=b.id확인
where a.mi그룹!=b.MI그룹
)v
select * from [dbo].[MIv2#1과MIv1#0$]

select * from [dbo].[인바디분석자료0429]
group by 미병그룹


select  count(*), mi그룹 from [dbo].[맥분석자료160428]
group by mi그룹

select  count(*), mi그룹 from [dbo].[혈액분석자료160428]
group by mi그룹





select * from [dbo].[인바디분석자료0429]