--dataset generation made by 2015.05.11 from MYP

--���ϸ� : 02. �̷��� �̺� ��� �����ڷ�_150414(Ż���� ����).xlsx
select * from [dbo].['��������(Ȱ��_üǥ_�Ƿ�_�밢_ü��)$'] where visit in ('D1','D3','D8') --390
select * from [dbo].[�̺�$] where visit in ('D1','D3','D8') --390
select * from ��������$  where visit in ('D1','D3','D8') --390
select * from ������$  where visit in ('D1','D3','D8') --390
select * from ��ȭ$ where visit in ('D1','D3','D8') --390
select * from ����$ where visit in ('D1','D3','D8') --390
select * from �ɸ���Ʈ����$ where visit in ('D1','D3','D8')--390
select * from ���$ where visit in ('D1','D3','D8')--390
select * from ����$ where visit in ('D1','D3','D8')--390
select * from �Ƿ�$ where visit in ('D1','D3','D8')--390
select * from ['�����к���(������)$'] where visit in ('D1','D3','D8') --388
select * from �����м���$ where visit in ('D1','D3','D8')--390
select * from ['����(NEO-PI)$']  where visit in ('D1','D3','D8') --260
select * from �α�����$ where visit in ('D1','D3','D8') --130
select * from ü������$ where visit in ('D1','D3','D8') --129


--�����͸� import ���Ѽ�, �̺������� �з� �ٽ� �ֱ�.
--���ϸ� : �̷��ι̺�_�������ѹ�Ż_�̺���������_150424_����.xlsx

alter table ['��������(Ȱ��_üǥ_�Ƿ�_�밢_ü��)$'] drop column [F48]
drop table #tmp 
select * into #tmp from (

select a.*,b.�̺�����,b.�̺��׷� from ['��������(Ȱ��_üǥ_�Ƿ�_�밢_ü��)$'] a inner join [�̺������׷�_����$] b 
	  on a.�����_��Ϲ�ȣ=b.[����� ��Ϲ�ȣ] and a.Visit=b.Visit
)v



drop table #tmp2
select * into #tmp2 from (

select a.*,[��������]
      ,[����1]
      ,[����2]
      ,[����3]
      ,[����4]
      ,[����5]
      ,[����6]
      ,[����7]
      ,[����8]
      ,[����9]
      ,[����10]
      ,[����11]
      ,[����12]
      ,[����1]
      ,[����2]
      ,[����3]
      ,[����4]
      ,[����5]
      ,[����6]
      ,[����7]
      ,[����8]
      ,[����9]
      ,[����10]
      ,[����11]
      ,[����12]
      ,[����13]
      ,[����14]
      ,[���1]
      ,[���2]
      ,[���3]
      ,[���4]
      ,[���5]
      ,[���6]
      ,[���7]
      ,[���8]
      ,[���9]
      ,[���10]
      ,[���11]
      ,[���12]
      ,[���13]
      ,[���14]
      ,[�ѿ�1]
      ,[�ѿ�2]
      ,[�ѿ�3]
      ,[�ѿ�4]
      ,[�ѿ�5]
      ,[�ѿ�6]
      ,[�ѿ�7]
      ,[�ѿ�8]
      ,[�ѿ�9]
      ,[�ѿ�10]
      ,[�ѿ�11]
      ,[�ѿ�12]
      ,[�ѿ�13]
      ,[�ѿ�14]
      ,[�ѿ�15]
      ,[�ѿ�16]
      ,[�ѿ�17]
      ,[�ѿ�18]
      ,[�ѿ�19]
      ,[�ѿ�20]
      ,[�ѿ�21]
      ,[�ѿ�22]
      ,[�ѿ�23]
       from #tmp a inner join [dbo].[��������$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v

drop table #tmp3
select * into #tmp3 from (
select a.*, [SF12_YN]
      ,[�Ϲݰǰ�]
      ,[��ü���1]
      ,[��ü���2]
      ,[��ü����1]
      ,[��ü����2]
      ,[��������1]
      ,[��������2]
      ,[����]
      ,[���Űǰ�1]
      ,[Ȱ��]
      ,[���Űǰ�2]
      ,[��ȸ���]
      ,[EQ5D_VAS]
      ,[SF12_PCS]
      ,[SF12_MCS]
      ,[EQ5D_Values] from #tmp2 a inner join [dbo].[������$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit

)v


drop table #tmp4
select * into #tmp4 from (

select a.* ,[�Ŀ�_����]
      ,[�Ŀ����]
      ,[�������]
      ,[������]
      ,[���ļ����]
      ,[GSRS_Score] from #tmp3 a inner join [dbo].[��ȭ$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v



drop table #tmp5
select * into #tmp5 from (

select a.*  ,[PSQI_Total]
      ,[SMH_Score] from #tmp4 a inner join [dbo].[����$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v


drop table #tmp6
select * into #tmp6 from (

select a.*  ,[PWI_Socre] from #tmp5 a inner join  [dbo].[�ɸ���Ʈ����$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v


drop table #tmp7
select * into #tmp7 from (

select a.* ,[BDI_Score] from #tmp6 a inner join [dbo].[���$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v



drop table #tmp8
select * into #tmp8 from (

select a.*  ,[����ô��]
      ,[����_��ü]
      ,[����_�ɸ�] from #tmp7 a inner join [dbo].[����$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v




drop table #tmp9
select * into #tmp9 from (
select a.* ,[CFS_score] from #tmp8 a inner join [dbo].[�Ƿ�$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v





drop table #tmp10

select * into #tmp10 from (

select a.* 
      ,[����1]
      ,[����2]
      ,[����3]
      ,[����4]
      ,[����5]
      ,[����6]
      ,[�Ļ�_��Ģ��]
      ,[�Ļ�_Ƚ��]
      ,[�Ļ�_�Ļ緮����]
      ,[�Ļ�_�Ļ緮]
      ,[�Ļ�_�ð�]
      ,[��ȭ_����]
      ,[��ȭ_�Ը�]
      ,[��1]
      ,[��2]
      ,[��_�Ӹ��;�]
      ,[��_��]
      ,[��_�������ܵ����]
      ,[��_��]
      ,[��_��]
      ,[��_��]
      ,[��_��Ÿ����]
      ,[��_��ü]
      ,[�뺯_����]
      ,[�뺯_Ƚ��_ȸ]
      ,[�뺯_Ƚ��_��]
      ,[�뺯_�ð�]
      ,[�뺯_���]
      ,[�뺯_��ڰ�]
      ,[�Һ�_Ƚ��_����]
      ,[�Һ�_Ƚ��_�����]
      ,[�Һ�_����]
      ,[�Һ�_Ź�ϴ�]
      ,[�Һ�_���̸���]
      ,[�Һ�_�����]
      ,[�Һ�_�Ӵ�]
      ,[�Һ�_��ǰ��]
      ,[��Ÿ_��������]
      ,[��Ÿ_�����µ�]
      ,[�����м���2]
      ,[����1]
      ,[����2]
      ,[����3]
      ,[����4]
      ,[�Ŀ��ȭ5]
      ,[�Ŀ��ȭ6]
      ,[�Ŀ��ȭ7]
      ,[�Ŀ��ȭ8]
      ,[�Ŀ��ȭ9]
      ,[�Ŀ��ȭ10]
      ,[�Ŀ��ȭ11]
      ,[�Ŀ��ȭ12]
      ,[�뺯13]
      ,[�뺯14]
      ,[�뺯15]
      ,[�뺯16]
      ,[�뺯17]
      ,[�뺯18]
      ,[�Һ�19]
      ,[�Һ�20]
      ,[�Һ�21]
      ,[�Һ�22]
      ,[��23]
      ,[��24]
      ,[��25]
      ,[��26]
      ,[�ѿ�27]
      ,[�ѿ�28]
      ,[��Ÿ29]
      ,[��Ÿ30]
      ,[��Ÿ31]
      ,[��Ÿ32]
      ,[��Ÿ33]
      ,[��Ÿ34]
      ,[��������35]
      ,[��������36]
      ,[��������37]
      ,[��������38]
      ,[��������39]
      ,[��������40]
      ,[��������41]
      ,[��������42]
      ,[��������43]
      ,[��������44]
      ,[��������45]
      ,[��������46]
 from #tmp9 a inner join [dbo].[�����м���$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v


--���⼭ ���� �����͵��� ���ڶ� left join ���� ..

drop table #tmp11
select * into #tmp11 from (

select a.*,[���������ܽ��࿩��]
      ,[����]
      ,[����]
      ,[���]
      ,[��]
      ,[��]
      ,[�ǰ���������] from #tmp10 a left join  ['�����к���(������)$'] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v

drop table #tmp12
select * into #tmp12 from (
select a.*,[�����з�]
      ,[�з�]
      ,[��Ȱ����_�ɾ��ִ�_�ð�]
      ,[��Ȱ����_�ɾ��ִ�_��]
      ,[��Ȱ����_�����ִ�_�ð�]
      ,[��Ȱ����_�����ִ�_��]
      ,[��Ȱ����_���ִ�_�ð�]
      ,[��Ȱ����_���ִ�_��]
      ,[�����ð�]
      ,[����_��_��]
      ,[����_��_��]
      ,[����_�ɾ��ִ�_�ð�]
      ,[����_�ɾ��ִ�_��]
      ,[����_���ִ�_�ð�]
      ,[����_���ִ�_��]
      ,[��ռ���] from (
select  [�����_��Ϲ�ȣ]
      ,[Visit]
      ,[case_����]
      ,[����]
      ,[����]
      ,[ü��]
      ,[�ִ�ü��]
      ,[BMI]
      ,[�������] from �α�����$ where visit in ('D0') ) a
	  inner join (select  [�����_��Ϲ�ȣ]
,[�����з�]
      ,[�з�]
      ,[��Ȱ����_�ɾ��ִ�_�ð�]
      ,[��Ȱ����_�ɾ��ִ�_��]
      ,[��Ȱ����_�����ִ�_�ð�]
      ,[��Ȱ����_�����ִ�_��]
      ,[��Ȱ����_���ִ�_�ð�]
      ,[��Ȱ����_���ִ�_��]
      ,[�����ð�]
      ,[����_��_��]
      ,[����_��_��]
      ,[����_�ɾ��ִ�_�ð�]
      ,[����_�ɾ��ִ�_��]
      ,[����_���ִ�_�ð�]
      ,[����_���ִ�_��]
      ,[��ռ���]
from �α�����$ where visit in ('D1')) b on a.[�����_��Ϲ�ȣ]=b.[�����_��Ϲ�ȣ]
)v


drop table #tmp13
select * into #tmp13 from (
select a.*,[����]
      ,[����]
      ,[ü��]
      ,[�ִ�ü��]
      ,[BMI]
      ,[�����з�]
      ,[�з�]
      ,[��Ȱ����_�ɾ��ִ�_�ð�]
      ,[��Ȱ����_�ɾ��ִ�_��]
      ,[��Ȱ����_�����ִ�_�ð�]
      ,[��Ȱ����_�����ִ�_��]
      ,[��Ȱ����_���ִ�_�ð�]
      ,[��Ȱ����_���ִ�_��]
      ,[�����ð�]
      ,[����_��_��]
      ,[����_��_��]
      ,[����_�ɾ��ִ�_�ð�]
      ,[����_�ɾ��ִ�_��]
      ,[����_���ִ�_�ð�]
      ,[����_���ִ�_��]
      ,[��ռ���] from #tmp11 a inner join  #tmp12 b on a.[�����_��Ϲ�ȣ]=b.[�����_��Ϲ�ȣ]
)v




drop table #tmp14
select * into #tmp14 from (

select a.* ,[NEO_�Ű���]
      ,[NEO_���⼺]
      ,[NEO_���漺]
      ,[NEO_������]
      ,[NEO_���Ǽ�] from #tmp13 a inner join ['����(NEO-PI)$'] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ where b.Visit='D1'
)v

drop table #tmp15
select * into #tmp15 from (
select a.*,ü������ from #tmp14 a left join [dbo].[ü������$] b on   a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ
)v


delete from #tmp15 where �̺��׷�='error'


drop table �̺�������final
select * into �̺�������final from (select *,�̺�fg=case when �̺��׷�=1 then '�ǰ���'
                                                     when �̺��׷�=2 then '�̺�1' when �̺��׷�=3 then '�̺�2' end from #tmp15 )v


drop table #tmp16
select * into #tmp16 from (
select a.*
      ,[a_��������]
      ,[a_�����ܾ�]
      ,[a_ü����]
      ,[a_�ܹ���]
      ,[a_������]
      ,[a_������]
      ,[a_�����淮]
      ,[a_�����ִ�_��������]
      ,[a_ü����]
      ,[a0_ü��_]
      ,[aa_��ݱٷ�]
      ,[aa__BMI]
      ,[aa_ü�����]
      ,[aa_WHR]
      ,[aa_������_������]
      ,[aa_���ȱ�����]
      ,[aa_����_������]
      ,[aa_����_�ٸ�_������]
      ,[aa_��_�ٸ�_������]
      ,[a0_������_����_ECF__TBF_]
      ,[aa_��_��_����_ECF__TBF_]
      ,[aa_����_����_ECF__TBF_]
      ,[aa_�����ٸ�_����_ECF__TBF_]
      ,[aa_�޴ٸ�_����_ECF__TBF_]
      ,[aa_��ü_����_ECF__TBF_]
      ,[aa_������_����_ECW__TBW_]
      ,[aa_��_��_����_ECW__TBW_]
      ,[aa_����_����_ECW__TBW_]
      ,[aa_�����ٸ�_����_ECW__TBW_]
      ,[a0_�޴ٸ�_����_ECW__TBW_]
      ,[aa_��ü_����_ECW__TBW_]
      ,[aa_��������_�ܸ���]
      ,[aa_�񸸵�]
      ,[aa_ü������]
      ,[aa_BMC]
      ,[aa_BMR]
      ,[aa_��ѷ�]
      ,[aa_�����ѷ�]
      ,[aa_���εѷ�]
      ,[a0_������_�ѷ�]
      ,[aa_�����ȵѷ�]
      ,[aa_���ȵѷ�]
      ,[aa_������_�����_�ѷ�]
      ,[aa_����_�����_�ѷ�]
      ,[aa_��_����_�ѷ�]
      ,[aa_����ü��]
      ,[aa_ü��������]
      ,[aa_ü����_������]
      ,[aa_����_������]
      ,[a0_��ü�ߴ�����]
      ,[aa_akhz_RA__]
      ,[aa_akhz_LA]
      ,[aa_akhz_TR]
      ,[aa_akhz_RL]
      ,[aa_akhz_LL]
      ,[aa_akhz_RA]
      ,[aa_akhz_LA1]
      ,[aa_akhz_TR1]
      ,[aa_akhz_RL1]
      ,[a0_akhz_LL]
      ,[aa_a0khz_RA]
      ,[aa_a0khz_LA]
      ,[aa_a0khz_TR]
      ,[aa_a0khz_RL]
      ,[aa_a0khz_LL]
      ,[aa_aa0khz_RA]
      ,[aa_aa0khz_LA]
      ,[aa_aa0khz_TR]
      ,[aa_aa0khz_RL]
      ,[a0_aa0khz_LL]
      ,[aa_a00khz_RA]
      ,[aa_a00khz_LA]
      ,[aa_a00khz_TR]
      ,[aa_a00khz_RL]
      ,[aa_a00khz_LL]
      ,[aa_aMhz_RA]
      ,[aa_aMhz_LA]
      ,[aa_aMhz_TR]
      ,[aa_aMhz_RL]
      ,[a0_aMhz_LL]
      ,[aa_akhz_RA__�����Ͻ�]
      ,[aa_akhz_LA2]
      ,[aa_akhz_TR2]
      ,[aa_akhz_RL2]
      ,[aa_akhz_LL1]
      ,[aa_a0khz_RA1]
      ,[aa_a0khz_LA1]
      ,[aa_a0khz_TR1]
      ,[aa_a0khz_RL1]
      ,[a0_a0khz_LL]
      ,[aa_aa0khz_RA1]
      ,[aa_aa0khz_LA1]
      ,[aa_aa0khz_TR1]
      ,[aa_aa0khz_RL1]
      ,[aa_aa0khz_LL]
      ,[aa_������_����_ǥ�ع���_���Ѱ�]
      ,[aa_������_����_ǥ�ع���_���Ѱ�]
      ,[aa_������_����_ǥ�ع���_���Ѱ�]
      ,[aa_������_����_ǥ�ع���_���Ѱ�]
      ,[a00_�ܹ���_ǥ�ع���_���Ѱ�]
      ,[a0a_�ܹ���_ǥ�ع���_���Ѱ�]
      ,[a0a_������_ǥ�ع���_���Ѱ�]
      ,[a0a_������_ǥ�ع���_���Ѱ�]
      ,[a0a_ü���淮_ǥ�ع���_���Ѱ�]
      ,[a0a_ü���淮_ǥ�ع���_���Ѱ�]
      ,[a0a_ü��_ǥ�ع���_���Ѱ�]
      ,[a0a_ü��_ǥ�ع���_���Ѱ�]
      ,[a0a_��ݱٷ�_ǥ�ع���_���Ѱ�]
      ,[a0a_��ݱٷ�_ǥ�ع���_���Ѱ�]
      ,[aa0_ü���淮_ǥ�ع���_���Ѱ�]
      ,[aaa_ü���淮_ǥ�ع���_���Ѱ�]
      ,[aaa_BMI_ǥ�ع���_���Ѱ�]
      ,[aaa_BMIǥ�ع���_���Ѱ�]
      ,[aaa_ü�����_ǥ�ع���_���Ѱ�]
      ,[aaa_ü�����_ǥ�ع���_���Ѱ�]
      ,[aaa_WHR_ǥ�ع���_���Ѱ�]
      ,[aaa_WHR_ǥ�ع���_���Ѱ�]
      ,[aaa_�����]
      ,[aaa_�����1]
      ,[aa0_�����]
      ,[aaa_�����2]
      ,[aaa_�����3]
      ,[aaa_�����4]
      ,[aaa_�����5]
      ,[aaa_�����6]
      ,[aaa_�����7]
      ,[aaa_�����8]
      ,[aaa_�����9]
      ,[aaa_�����10]
      ,[aa0_�����1]
      ,[aaa_�����11]
      ,[aaa_�����12]
      ,[aaa_�����13]
      ,[aaa_�����14]
      ,[aaa_�����15]
      ,[aaa_�����16]
      ,[aaa_������_������]
      ,[aaa_���ȱ�����]
      ,[aaa_����_������]
      ,[aa0_����_�ٸ�_������]
      ,[aaa_��_�ٸ�_������]


 from �̺�������final a inner join [dbo].[�ιٵ�$] b  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit

	  )v

	  
drop table #tmp17
select * into #tmp17 from (

select a.*, 
[�����Ű�_Ȱ����]
      ,[�����Ű�_Ȱ����_����]
      ,[�����Ű�_������]
      ,[�����Ű�_������_����]
      ,[��Ʈ����_���׵�]
      ,[��Ʈ����_���׵�_����]
      ,[��Ʈ����_����]
      ,[��Ʈ����_����_����]
      ,[�Ƿε�]
      ,[�Ƿε�_����]
      ,[���_�ɹڵ���]
      ,[���_�ɹڵ���_����]
      ,[����_������]
      ,[����_������_����]
      ,[�̻�_�ɹڵ���]
      ,[Sdnn]
      ,[Psi]
      ,[Tp]
      ,[Vlf]
      ,[Lf]
      ,[Hf]
      ,[LfNorm]
      ,[HfNorm]
      ,[Lf/Hf]
      ,[Rmssd]
      ,[Apen]
      ,[Srd]
      ,[Tsrd]
      ,[Tp(ln)]
      ,[Vlf(ln)]
      ,[Lf(ln)]
      ,[Hf(ln)] from #tmp16 a inner join [HRV_HRVresult$] b 
	  on a.�����_��Ϲ�ȣ=b.�����_��Ϲ�ȣ and a.Visit=b.Visit
)v

drop table #tmp18
select * into #tmp18 from (

select a.* ,[Wave_Type]
      ,[Vessel_Status_Score]
      ,[DPI(�̺и�������)]
      ,[�̺и�������_����]
      ,[SP(���Ⱝ��)]
      ,[���Ⱝ��_����]
      ,[BVT(������_ź����)]
      ,[������_ź����_����]
      ,[RBV(������)]
      ,[������_����]
      ,[VCT(����ð�)]
      ,[HR(�ɹڼ�)]
      ,[�ɹڼ�_����] from #tmp17 a inner join  [dbo].[HRV_APGresult$] b 
	  on a.�����_��Ϲ�ȣ=b.����ڵ�Ϲ�ȣ and a.Visit=b.Visit
)v



drop table #tmp19
select * into #tmp19 from (

select a.* ,[SOS]
      ,[OI]
      ,[Tscore]
      ,[Zscore] from #tmp18 a inner join [dbo].[��е�$] b 
	  on a.�����_��Ϲ�ȣ=b.����ڵ�Ϲ�ȣ and a.Visit=b.Visit
)v

drop table �̺�������final



select * into [�̺�������final] from 
(select *
,Forehead2hipd1 = foreheadSized1/ gokkolSized1
,Neck2hipd1 = neckSized1/ gokkolSized1
,Axillary2hipd1 = armpitCircumferenced1/ gokkolSized1
,Chest2hipd1 = chestSized1/ gokkolSized1
,Rib2hipd1 = ribSized1/ gokkolSized1
,Waist2hipd1 = waistSized1/ gokkolSized1
,Pelvic2Hipd1 = iliacSized1/ gokkolSized1
,Forehead2pelvicd1 = foreheadSized1/ iliacSized1
,Neck2pelvicd1 = neckSized1/ iliacSized1
,Axillary2pelvicd1 = armpitCircumferenced1/ iliacSized1
,Chest2pelvicd1 = chestSized1/ iliacSized1
,Rib2pelvicd1 = ribSized1/ iliacSized1
,Waist2pelvicd1 = waistSized1/ iliacSized1
,Forehead2waistd1 = foreheadSized1/ waistSized1
,Neck2waistd1 = neckSized1/ waistSized1
,Axillary2waistd1 = armpitCircumferenced1/ waistSized1
,Chest2waistd1 = chestSized1/ waistSized1
,Rib2waistd1 = ribSized1/ waistSized1
,Forehead2ribd1 = foreheadSized1/ ribSized1
,Neck2ribd1 = neckSized1/ ribSized1
,Axillary2ribd1 = armpitCircumferenced1/ ribSized1
,Chest2ribd1 = chestSized1/ ribSized1
,Forehead2chestd1 = foreheadSized1/ chestSized1
,Neck2chestd1 = neckSized1/ chestSized1
,Axillary2chestd1 = armpitCircumferenced1/ chestSized1
,Forehead2axillaryd1 = foreheadSized1/ armpitCircumferenced1
,Neck2axillaryd1 = neckSized1/ armpitCircumferenced1
,Forehead2neckd1 = foreheadSized1/ neckSized1
,Waist2heightd1 = ü��/ ����

,BCM_BW=aa_ü������/ü��
,��������_�����ܾ�=a_��������/a_�����ܾ�
,�̺��з�fi=�̺�fg
,�̺�����fi=�̺����� 
,ü������fi=ü������

from #tmp19)v

select count(*),ü������KS15 from [�̺�������final] 
WHERE VISIT='D1'
group by ü������KS15 

begin tran
update �̺�������final set �̺�fg = case when �̺��׷�=1 then '�ǰ���'
                                                     when �̺��׷�=2 then '�̺�1' when �̺��׷�=3 then '�̺�2' end
commit
sp_help �̺�������final

SELECT * INTO #TMP FROM (

select A.*,B.KS15 AS ü������KS15 from �̺�������final A LEFT JOIN [ü������KS15$] B ON A.�����_��Ϲ�ȣ=B.[����� ��Ϲ�ȣ]

) A

drop table �̺�������final

select * into �̺�������final from #TMP

select ���� from �̺�������final group by ����

-------0608�Ͽ� ��� ���̺� loading 
select * from [dbo].[���$]

drop table �̺�������final_����߰�
select * into �̺�������final_����߰� from (
select b.* ,[Forehead2hipd1]
      ,[Neck2hipd1]
      ,[Axillary2hipd1]
      ,[Chest2hipd1]
      ,[Rib2hipd1]
      ,[Waist2hipd1]
      ,[Pelvic2Hipd1]
      ,[Forehead2pelvicd1]
      ,[Neck2pelvicd1]
      ,[Axillary2pelvicd1]
      ,[Chest2pelvicd1]
      ,[Rib2pelvicd1]
      ,[Waist2pelvicd1]
      ,[Forehead2waistd1]
      ,[Neck2waistd1]
      ,[Axillary2waistd1]
      ,[Chest2waistd1]
      ,[Rib2waistd1]
      ,[Forehead2ribd1]
      ,[Neck2ribd1]
      ,[Axillary2ribd1]
      ,[Chest2ribd1]
      ,[Forehead2chestd1]
      ,[Neck2chestd1]
      ,[Axillary2chestd1]
      ,[Forehead2axillaryd1]
      ,[Neck2axillaryd1]
      ,[Forehead2neckd1]
      ,[Waist2heightd1]
      ,[BCM_BW]
      ,[��������_�����ܾ�]
      ,[�̺��з�fi]
      ,[�̺�����fi]
      ,[ü������fi] from �̺�������final a inner join [���$] b on a.�����_��Ϲ�ȣ=b.����ڵ�Ϲ�ȣ and a.Visit=b.Visit
)v


 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '�̺�������final_����߰�'
 and column_name like '%�ܵ�%'

 
 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '�̺�������final'
 and column_name like '%�ܵ�%'

select * from [dbo].['��������(Ȱ��_üǥ_�Ƿ�_�밢_ü��)$']

select * into [dbo].[�̺�������final_������ǥ2] from 
(
 select 
 a.Apen
 ,(a.aa_ü������/ü��) as BCMü�ߺ�
  ,a.a_������
 ,a.aa_��������_�ܸ���
 ,a.a_�ܹ���
 ,a.a_������
 ,a.aa_��ü_����_ECW__TBW_
,a.aa_��ü_����_ECF__TBF_
,a.a_�����淮
,a.aa_ü������
,a.a_ü����
 ,a.a_ü����
,a.aa_ü�����
,a.a_�����ִ�_��������
,a.[��������_�����ܾ�]
,a.a_��������
,a.aa_��ݱٷ�
,a.a_�����ܾ�
 ,ECI,ECO___  as ECO_per,ecri, esi
,a.�̻�_�ɹڵ���
,(chestSized1/ü��) as �ܵ���̵ѷ�ü�ߺ�
,(gokkolSized1/ü��) as ���ѷ�ü�ߺ�
,(neckSized1/ü��) as ��ѷ�ü�ߺ�
,(foreheadSized1/ü��) as �̸�ü�ߺ�
,(�Ƿ�_��1+�Ƿ�_��2+�Ƿ�_��1+�Ƿ�_��2)/4 as �Ƿ����
,(üǥ_�չٴ�_��1+üǥ_�չٴ�_��2+üǥ_�չٴ�_��1+üǥ_�չٴ�_��2)/4 as üǥ���
,(�밢_���_��1+�밢_���_��2+�밢_���_��1+�밢_���_��2)/4 as �밢���

,VO2_Peak
,METS_RC
,VE_LT
,VE_RC
,VE_Peak
,VT_LT
,VT_RC
,VT_Peak
,VO2_HR_LT��
,VO2_HR_RC��
,VO2_HR_Peak��
,VCO2_LT
,VCO2_RC
,VCO2_Peak
,VE_VCO2��
 
 ,����,
a.[�̺��з�fi],
a.[�̺�����fi],
a.[ü������fi]

from �̺�������final a inner join �̺�������final_����߰� b on a.[�����_��Ϲ�ȣ] =b.[����ڵ�Ϲ�ȣ] and a.Visit=b.Visit
inner join  [��緮�м��ڷ�$] c on a.[�����_��Ϲ�ȣ] =c.[����� ��Ϲ�ȣ] and a.Visit=c.Visit
) v

select * from [dbo].['4-7data$']

select [����ڵ�Ϲ�ȣ],[Visit],
from �̺�������final_����߰�

select * from [��緮�м��ڷ�$]

select 
[����� ��Ϲ�ȣ],[Visit]
VO2_Peak
,METS_RC
,VE_LT
,VE_RC
,VE_Peak
,VT_LT
,VT_RC
,VT_Peak
,VO2_HR_LT��
,VO2_HR_RC��
,VO2_HR_Peak��
,VCO2_LT
,VCO2_RC
,VCO2_Peak
,VE_VCO2��
 from [dbo].[��緮�м��ڷ�$]




  

 select EQ5D_VAS from �̺�������final
 select * from �̺�������final_����߰�

 drop table �̺�������final_������ǥ 
 select * into �̺�������final_������ǥ from (

 select  ECI,ECO___  ,ecri, esi,RMR_ü��,[aa_��ݱٷ�],[a_�ܹ���]
,[a_������]
,[aa_��������_�ܸ���]
,[a_������]
,[a_�����ִ�_��������],b.*



 from 
 (
select [����ڵ�Ϲ�ȣ],[Visit], Apen,aa_BMC,aa_BMR,ECI,ECO___ ,ecri, esi,RMR/a0_ü��_ as RMR_ü��,[aa_��ݱٷ�],[a_�ܹ���]
,[a_������]
,[aa_��������_�ܸ���]
,[a_������],
[a_�����ִ�_��������]

 from �̺�������final_����߰�
) a inner join (
select
[�����_��Ϲ�ȣ],
[Visit]
 --,BDI_Score
--,cfs_score
--,[EQ5D_Values]
--,[GSRS_Score]
--,NEO_���Ǽ�
--,NEO_�Ű���
--,NEO_���⼺
--,PSQI_Total
--,PWI_Socre
--,SF12_MCS
--,SF12_PCS
--,SMH_Score
--,�ǰ���������
--,aa_�ܵ���̵ѷ�
--,���,����
,[aa_��ѷ�]/ü�� as ��ѷ�_ü��
--,EQ5D_VAS
,[��������_�����ܾ�]
,a_��������
,�̻�_�ɹڵ���
,aa_��ü_����_ECW__TBW_
,aa_��ü_����_ECF__TBF_
,a_�����淮
,aa_ü������
,a_ü����
--,ü��
,a_ü����
,aa_ü�����
--,��
--,����
,(�Ƿ�_��1+�Ƿ�_��2+�Ƿ�_��1+�Ƿ�_��2)/4 as �Ƿ����
,(üǥ_�չٴ�_��1+üǥ_�չٴ�_��2+üǥ_�չٴ�_��1+üǥ_�չٴ�_��2)/4 as üǥ���
,(�밢_���_��1+�밢_���_��2+�밢_���_��1+�밢_���_��2)/4 as �밢���
 ,����,
[�̺��з�fi],
[�̺�����fi],[ü������fi]
 from �̺�������final ) b on a.[����ڵ�Ϲ�ȣ]=b.[�����_��Ϲ�ȣ] and a.[Visit]=b.Visit

 )v  


 select * from [�̺�������final]





 --���õ� ��ǥ 3

 
 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '�̺�������final_����߰�'
 and column_name like '%es%'

 
 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '�̺�������final'
 and column_name like '%whr%'

drop table [�̺�������final_������ǥ3]
select * into [dbo].[�̺�������final_������ǥ3] from 
(
 select  a.[�����_��Ϲ�ȣ],
 ���������
 ,�̿ϱ�����
 ,aa__BMI
 ,a.aa_WHR
 ,foreheadSized1
 ,neckSized1
 ,armpitCircumferenced1
 ,chestSized1
 ,ribSized1
 ,waistSized1
 ,iliacSized1
 ,gokkolSized1
,(foreheadSized1/ü��) as �̸�ü�ߺ�
,(neckSized1/ü��) as ��ü�ߺ�
,(armpitCircumferenced1/ü��) as �ܵ���̵ѷ�ü�ߺ�
,(chestSized1/ü��) as ����ü�ߺ�
,(ribSized1/ü��) as ����ü�ߺ�
,(waistSized1/ü��) as �㸮ü�ߺ�
,(iliacSized1/ü��) as ���ü�ߺ�
,(gokkolSized1/ü��) as ���ü�ߺ�
,(�Ƿ�_��1+�Ƿ�_��2)/2 as �Ƿ¿�
,(�Ƿ�_��1+�Ƿ�_��2)/2 as �Ƿ���
,(üǥ_�չٴ�_��1+üǥ_�չٴ�_��2)/2 as üǥ_�չٴ�_��
,(üǥ_�չٴ�_��1+üǥ_�չٴ�_��2)/2 as üǥ_�չٴ�_��
,((üǥ_�̸�1+üǥ_�̸�2)/2-(üǥ_�չٴ�_��1+üǥ_�չٴ�_��2)/2) as �̸�_�չٴ�_��_��
,((üǥ_�̸�1+üǥ_�̸�2)/2-(üǥ_�չٴ�_��1+üǥ_�չٴ�_��2)/2) as �̸�_�չٴ�_��_��
,(�밢_���_��1+�밢_���_��2)/2 as �밢_���_��
,(�밢_���_��1+�밢_���_��2)/2 as �밢_���_��
,(�밢_���_��1+�밢_���_��2)/2 as �밢_���_��

,a.a_��������
,a.a_�����ܾ�
,a.[��������_�����ܾ�] as �������׿ܾ׺�
,a.a_ü����
 ,a.a_�ܹ���
 ,a.a_������
 ,a.a_������
,a.a_�����淮
,a.a_�����ִ�_��������
,a.a0_ü��_
,a.aa_��ݱٷ�
,a.aa_ü������
 ,(a.aa_ü������/ü��) as BCMü�ߺ�
 ,a.aa_BMR
 ,a.aa_ü�����
 
 ,a.aa_��ü_����_ECW__TBW_
,a.aa_��ü_����_ECF__TBF_

 ,a.Apen
 ,a.[Lf/Hf] as lf_hf
 ,a.��Ʈ����_���׵�
     ,ECI
	 ,ECO

,ECO___  as ECO_per,ecri, esv,esi


,VO2_Peak
,METS_RC
,VE_LT
,VE_RC
,VE_Peak
,VT_LT
,VT_RC
,VT_Peak
,VO2_HR_LT��
,VO2_HR_RC��
,VO2_HR_Peak��
,VCO2_LT
,VCO2_RC
,VCO2_Peak
,VE_VCO2��
 ,����,
a.[�̺��з�fi],
a.[�̺�����fi],
a.[ü������fi]

from �̺�������final a inner join �̺�������final_����߰� b on a.[�����_��Ϲ�ȣ] =b.[����ڵ�Ϲ�ȣ] and a.Visit=b.Visit
inner join  [��緮�м��ڷ�$] c on a.[�����_��Ϲ�ȣ] =c.[����� ��Ϲ�ȣ] and a.Visit=c.Visit
) v



--20150622 ü�����ܰ��� ����Ǿ� �ٽ�, �غ���..
select * into [�̺�������final_������ǥ3_ü��������] from (
select a.*,b.ü������ from [�̺�������final_������ǥ3] a inner join [dbo].[ü������$] b on a.�����_��Ϲ�ȣ=b.[����� ��Ϲ�ȣ]
) v

alter table [�̺�������final_������ǥ3_ü��������] drop column �����_��Ϲ�ȣ
alter table [�̺�������final_������ǥ3_ü��������] drop column ü������fi
sp_rename '�̺�������final_������ǥ3_ü��������.ü������', 'ü������fi'




--20150622 ü�����ܰ��� ����Ǿ� �ٽ�, �غ���..

select * into [�̺�������final_������ǥ4_ü��������KS15] from (
select a.*,b.ks15 from [�̺�������final_������ǥ3] a inner join [dbo].[ü������KS15$] b on a.�����_��Ϲ�ȣ=b.[����� ��Ϲ�ȣ]
) v


alter table [�̺�������final_������ǥ4_ü��������KS15] drop column �����_��Ϲ�ȣ
alter table [�̺�������final_������ǥ4_ü��������KS15] drop column ü������fi
sp_rename '�̺�������final_������ǥ4_ü��������KS15.KS15', 'ü������fi'




select ü������fi,count(*) from [�̺�������final_������ǥ3_ü��������]
group by ü������fi

select ü������fi,count(*) from [�̺�������final_������ǥ4_ü��������KS15]
group by ü������fi