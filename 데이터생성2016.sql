select * from [dbo].[MI_v2_��õ����_0324]

drop table mi7��ô��10����_2
select * into mi7��ô��10����_2 from (
select a.*,b.mi_total_score,b.mi_total_group1 as mi_total_group3  from [dbo].[�ιٵ�770����_0308] a inner join [dbo].[mi7��ô��10����] b
on a.sub_id=b.Ȯ��id where v_d_s='1-0-0' and mi_total_group1 is not null
)v


select * from [dbo].[mi7��ô��10����_2]


select * from [dbo].[mi7��ô��4����]


--MI_V2�� ��õ,����,���� ���� �ִ� ��������(ID�� �̺��з�)

drop table mi7��ô��4����������õ����_2
select * into mi7��ô��4����������õ����_2 from (
select a.*,b.mi_total_score,b.mi_total_group3 as mi_total_group3  from [dbo].[�ιٵ�770����_0308] a inner join [dbo].[MI_v2$] b
on a.sub_id=b.Ȯ��id where v_d_s='1-0-0' and (mi_total_group3 is not null AND mi_total_group3!='')
)v


--�ߺ� Ȯ��
SELECT mi_total_group3,COUNT(*) FROM mi7��ô��4����������õ����_2
GROUP BY mi_total_group3 HAVING COUNT(*)>1



--4�� 6��..���ڷ� �̿�
drop table mac_data
select * into mac_data from (
select Ȯ��id,b.Energy,b.HR
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

from [dbo].[MI_v2$] a inner join  [HDresult$]  b on a.Ȯ��id=(b.id+'-'+b.V_D_S)
inner join [dbo].[Mh$] c on a.Ȯ��id=(c.id+'-'+c.V_D_S)
inner join [dbo].[Sh$] d on a.Ȯ��id=(d.id+'-'+d.V_D_S)
inner join [dbo].[APG$] e on a.Ȯ��id=(e.id+'-'+e.V_D_S)
where mi_total_Group3 in (1,2,3) and b.energy is not null

)v
select * from [dbo].[MI_v2$] 
select * from mac_data
select 'a'+'b'


select * from [dbo].[HDresult$]


--�����Ͱ� �߸��Ǽ� ȣ������ �� �����ͷ� �ٽ� �м� 2016-04-07 

drop table mi7��ô��4����������õ����_3
select * into mi7��ô��4����������õ����_3 from (
select a.*,b.mi_total_score,b.mi_total_group3 as mi_total_group3  from [dbo].[�ιٵ�770����_0407_hs] a inner join [dbo].[MI_v2$] b
on a.sub_id=b.Ȯ��id where a.[v_d_s]='1-0-0' and (mi_total_group3 is not null AND mi_total_group3!='')

)v

select * from [dbo].[mi7��ô��4����������õ����_2]



--2106
select * from [dbo].[MI_v2_mapping] where Ȯ�� like '%����%'  --�����̶�� ���ִ°��� 773 �� �������� 777��



drop table �����븸
select * into �����븸 from (

select a.*,b.mi_total_score,b.mi_total_group3 as mi_total_group3  from [dbo].[�ιٵ�770����_0407_hs] a inner join [dbo].[MI_v2_mapping] b
on a.sub_id=b.Ȯ��id where a.[v_d_s]='1-0-0' and (mi_total_group3 is not null AND mi_total_group3!='')
and b.Ȯ�� like '%����%'

)v


select * from �����븸

--2016-04-28 ���� �������� .. ���������� ���� ���ο� �����͵��� �̿��Ͽ� �м�
--������ version 2.1(227)�� v1(777)(�ǰ��� ����鸸, ��������� üũ�� ����ڵ���. ) �� ����.. 

select * from [dbo].[MIv2#1��MIv1#0$]

sp_rename '�м�����$', '�ιٵ�м�����160428'
sp_rename 'HRV�м��ڷ�$', 'HRV�м��ڷ�160428'
sp_rename '���׺м��ڷ�$', '���׺м��ڷ�160428'
sp_rename '�ƺм��ڷ�s$', '�ƺм��ڷ�160428'

select * from [MIv2#1��MIv1#0$]

select * into �ιٵ�м�����160428with�̺��з� from (

select * from �ιٵ�м�����160428 a inner join [dbo].[MIv2#1��MIv1#0$] b

on a.[�ιٵ� ID]=b.idȮ��
where a.mi�׷�!=b.MI�׷�
)v
select * from [dbo].[MIv2#1��MIv1#0$]

select * from [dbo].[�ιٵ�м��ڷ�0429]
group by �̺��׷�


select  count(*), mi�׷� from [dbo].[�ƺм��ڷ�160428]
group by mi�׷�

select  count(*), mi�׷� from [dbo].[���׺м��ڷ�160428]
group by mi�׷�





select * from [dbo].[�ιٵ�м��ڷ�0429]