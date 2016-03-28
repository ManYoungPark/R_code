--dataset generation made by 2015.05.11 from MYP

--파일명 : 02. 미래부 미병 기기 통합자료_150414(탈락자 제외).xlsx
select * from [dbo].['계측정보(활력_체표_악력_통각_체형)$'] where visit in ('D1','D3','D8') --390
select * from [dbo].[미병$] where visit in ('D1','D3','D8') --390
select * from 변증설문$  where visit in ('D1','D3','D8') --390
select * from 삶의질$  where visit in ('D1','D3','D8') --390
select * from 소화$ where visit in ('D1','D3','D8') --390
select * from 수면$ where visit in ('D1','D3','D8') --390
select * from 심리스트레스$ where visit in ('D1','D3','D8')--390
select * from 우울$ where visit in ('D1','D3','D8')--390
select * from 통증$ where visit in ('D1','D3','D8')--390
select * from 피로$ where visit in ('D1','D3','D8')--390
select * from ['한의학변증(전문가)$'] where visit in ('D1','D3','D8') --388
select * from 한의학설문$ where visit in ('D1','D3','D8')--390
select * from ['성격(NEO-PI)$']  where visit in ('D1','D3','D8') --260
select * from 인구학적$ where visit in ('D1','D3','D8') --130
select * from 체질진단$ where visit in ('D1','D3','D8') --129


--데이터를 import 시켜서, 미병점수와 분류 다시 넣기.
--파일명 : 미래부미병_수면제한박탈_미병점수재계산_150424_정리.xlsx

alter table ['계측정보(활력_체표_악력_통각_체형)$'] drop column [F48]
drop table #tmp 
select * into #tmp from (

select a.*,b.미병점수,b.미병그룹 from ['계측정보(활력_체표_악력_통각_체형)$'] a inner join [미병점수그룹_최종$] b 
	  on a.대상자_등록번호=b.[대상자 등록번호] and a.Visit=b.Visit
)v



drop table #tmp2
select * into #tmp2 from (

select a.*,[변증유무]
      ,[기허1]
      ,[기허2]
      ,[기허3]
      ,[기허4]
      ,[기허5]
      ,[기허6]
      ,[기허7]
      ,[기허8]
      ,[기허9]
      ,[기허10]
      ,[기허11]
      ,[기허12]
      ,[혈허1]
      ,[혈허2]
      ,[혈허3]
      ,[혈허4]
      ,[혈허5]
      ,[혈허6]
      ,[혈허7]
      ,[혈허8]
      ,[혈허9]
      ,[혈허10]
      ,[혈허11]
      ,[혈허12]
      ,[혈허13]
      ,[혈허14]
      ,[기울1]
      ,[기울2]
      ,[기울3]
      ,[기울4]
      ,[기울5]
      ,[기울6]
      ,[기울7]
      ,[기울8]
      ,[기울9]
      ,[기울10]
      ,[기울11]
      ,[기울12]
      ,[기울13]
      ,[기울14]
      ,[한열1]
      ,[한열2]
      ,[한열3]
      ,[한열4]
      ,[한열5]
      ,[한열6]
      ,[한열7]
      ,[한열8]
      ,[한열9]
      ,[한열10]
      ,[한열11]
      ,[한열12]
      ,[한열13]
      ,[한열14]
      ,[한열15]
      ,[한열16]
      ,[한열17]
      ,[한열18]
      ,[한열19]
      ,[한열20]
      ,[한열21]
      ,[한열22]
      ,[한열23]
       from #tmp a inner join [dbo].[변증설문$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v

drop table #tmp3
select * into #tmp3 from (
select a.*, [SF12_YN]
      ,[일반건강]
      ,[신체기능1]
      ,[신체기능2]
      ,[신체역할1]
      ,[신체역할2]
      ,[정서역할1]
      ,[정서역할2]
      ,[통증]
      ,[정신건강1]
      ,[활력]
      ,[정신건강2]
      ,[사회기능]
      ,[EQ5D_VAS]
      ,[SF12_PCS]
      ,[SF12_MCS]
      ,[EQ5D_Values] from #tmp2 a inner join [dbo].[삶의질$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit

)v


drop table #tmp4
select * into #tmp4 from (

select a.* ,[식욕_설문]
      ,[식욕상태]
      ,[허기정도]
      ,[포만감]
      ,[음식섭취양]
      ,[GSRS_Score] from #tmp3 a inner join [dbo].[소화$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v



drop table #tmp5
select * into #tmp5 from (

select a.*  ,[PSQI_Total]
      ,[SMH_Score] from #tmp4 a inner join [dbo].[수면$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v


drop table #tmp6
select * into #tmp6 from (

select a.*  ,[PWI_Socre] from #tmp5 a inner join  [dbo].[심리스트레스$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v


drop table #tmp7
select * into #tmp7 from (

select a.* ,[BDI_Score] from #tmp6 a inner join [dbo].[우울$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v



drop table #tmp8
select * into #tmp8 from (

select a.*  ,[통증척도]
      ,[통증_신체]
      ,[통증_심리] from #tmp7 a inner join [dbo].[통증$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v




drop table #tmp9
select * into #tmp9 from (
select a.* ,[CFS_score] from #tmp8 a inner join [dbo].[피로$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v





drop table #tmp10

select * into #tmp10 from (

select a.* 
      ,[성격1]
      ,[성격2]
      ,[성격3]
      ,[성격4]
      ,[성격5]
      ,[성격6]
      ,[식사_규칙적]
      ,[식사_횟수]
      ,[식사_식사량일정]
      ,[식사_식사량]
      ,[식사_시간]
      ,[소화_정도]
      ,[소화_입맛]
      ,[땀1]
      ,[땀2]
      ,[땀_머리와얼굴]
      ,[땀_목]
      ,[땀_가슴과겨드랑이]
      ,[땀_등]
      ,[땀_손]
      ,[땀_발]
      ,[땀_사타구니]
      ,[땀_전체]
      ,[대변_습관]
      ,[대변_횟수_회]
      ,[대변_횟수_일]
      ,[대변_시간]
      ,[대변_양상]
      ,[대변_긴박감]
      ,[소변_횟수_평상시]
      ,[소변_횟수_수면시]
      ,[소변_세기]
      ,[소변_탁하다]
      ,[소변_색이맑다]
      ,[소변_노랗다]
      ,[소변_붉다]
      ,[소변_거품뇨]
      ,[기타_추위더위]
      ,[기타_음수온도]
      ,[한의학설문2]
      ,[수면1]
      ,[수면2]
      ,[수면3]
      ,[수면4]
      ,[식욕소화5]
      ,[식욕소화6]
      ,[식욕소화7]
      ,[식욕소화8]
      ,[식욕소화9]
      ,[식욕소화10]
      ,[식욕소화11]
      ,[식욕소화12]
      ,[대변13]
      ,[대변14]
      ,[대변15]
      ,[대변16]
      ,[대변17]
      ,[대변18]
      ,[소변19]
      ,[소변20]
      ,[소변21]
      ,[소변22]
      ,[땀23]
      ,[땀24]
      ,[땀25]
      ,[땀26]
      ,[한열27]
      ,[한열28]
      ,[기타29]
      ,[기타30]
      ,[기타31]
      ,[기타32]
      ,[기타33]
      ,[기타34]
      ,[불편증상35]
      ,[불편증상36]
      ,[불편증상37]
      ,[불편증상38]
      ,[불편증상39]
      ,[불편증상40]
      ,[불편증상41]
      ,[불편증상42]
      ,[불편증상43]
      ,[불편증상44]
      ,[불편증상45]
      ,[불편증상46]
 from #tmp9 a inner join [dbo].[한의학설문$] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v


--여기서 부터 데이터들이 모자라서 left join 으로 ..

drop table #tmp11
select * into #tmp11 from (

select a.*,[전문가진단시행여부]
      ,[기허]
      ,[혈허]
      ,[기울]
      ,[한]
      ,[열]
      ,[건강상태점수] from #tmp10 a left join  ['한의학변증(전문가)$'] b 
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v

drop table #tmp12
select * into #tmp12 from (
select a.*,[직업분류]
      ,[학력]
      ,[생활형태_앉아있는_시간]
      ,[생활형태_앉아있는_분]
      ,[생활형태_누워있는_시간]
      ,[생활형태_누워있는_분]
      ,[생활형태_서있는_시간]
      ,[생활형태_서있는_분]
      ,[직무시간]
      ,[직무_총_시]
      ,[직무_총_분]
      ,[직무_앉아있는_시간]
      ,[직무_앉아있는_분]
      ,[직무_서있는_시간]
      ,[직무_서있는_분]
      ,[평균수입] from (
select  [대상자_등록번호]
      ,[Visit]
      ,[case_구분]
      ,[성별]
      ,[신장]
      ,[체중]
      ,[최대체중]
      ,[BMI]
      ,[생년월일] from 인구학적$ where visit in ('D0') ) a
	  inner join (select  [대상자_등록번호]
,[직업분류]
      ,[학력]
      ,[생활형태_앉아있는_시간]
      ,[생활형태_앉아있는_분]
      ,[생활형태_누워있는_시간]
      ,[생활형태_누워있는_분]
      ,[생활형태_서있는_시간]
      ,[생활형태_서있는_분]
      ,[직무시간]
      ,[직무_총_시]
      ,[직무_총_분]
      ,[직무_앉아있는_시간]
      ,[직무_앉아있는_분]
      ,[직무_서있는_시간]
      ,[직무_서있는_분]
      ,[평균수입]
from 인구학적$ where visit in ('D1')) b on a.[대상자_등록번호]=b.[대상자_등록번호]
)v


drop table #tmp13
select * into #tmp13 from (
select a.*,[성별]
      ,[신장]
      ,[체중]
      ,[최대체중]
      ,[BMI]
      ,[직업분류]
      ,[학력]
      ,[생활형태_앉아있는_시간]
      ,[생활형태_앉아있는_분]
      ,[생활형태_누워있는_시간]
      ,[생활형태_누워있는_분]
      ,[생활형태_서있는_시간]
      ,[생활형태_서있는_분]
      ,[직무시간]
      ,[직무_총_시]
      ,[직무_총_분]
      ,[직무_앉아있는_시간]
      ,[직무_앉아있는_분]
      ,[직무_서있는_시간]
      ,[직무_서있는_분]
      ,[평균수입] from #tmp11 a inner join  #tmp12 b on a.[대상자_등록번호]=b.[대상자_등록번호]
)v




drop table #tmp14
select * into #tmp14 from (

select a.* ,[NEO_신경증]
      ,[NEO_외향성]
      ,[NEO_개방성]
      ,[NEO_동조성]
      ,[NEO_성실성] from #tmp13 a inner join ['성격(NEO-PI)$'] b 
	  on a.대상자_등록번호=b.대상자_등록번호 where b.Visit='D1'
)v

drop table #tmp15
select * into #tmp15 from (
select a.*,체질진단 from #tmp14 a left join [dbo].[체질진단$] b on   a.대상자_등록번호=b.대상자_등록번호
)v


delete from #tmp15 where 미병그룹='error'


drop table 미병데이터final
select * into 미병데이터final from (select *,미병fg=case when 미병그룹=1 then '건강군'
                                                     when 미병그룹=2 then '미병1' when 미병그룹=3 then '미병2' end from #tmp15 )v


drop table #tmp16
select * into #tmp16 from (
select a.*
      ,[a_세포내액]
      ,[a_세포외액]
      ,[a_체수분]
      ,[a_단백질]
      ,[a_근육량]
      ,[a_무기질]
      ,[a_제지방량]
      ,[a_뼈에있는_무기질량]
      ,[a_체지방]
      ,[a0_체중_]
      ,[aa_골격근량]
      ,[aa__BMI]
      ,[aa_체지방률]
      ,[aa_WHR]
      ,[aa_오른팔_근육량]
      ,[aa_왼팔근육량]
      ,[aa_몸통_근육량]
      ,[aa_오른_다리_근육량]
      ,[aa_왼_다리_근육량]
      ,[a0_오른팔_부종_ECF__TBF_]
      ,[aa_왼_팔_부종_ECF__TBF_]
      ,[aa_몸통_부종_ECF__TBF_]
      ,[aa_오른다리_부종_ECF__TBF_]
      ,[aa_왼다리_부종_ECF__TBF_]
      ,[aa_전체_부종_ECF__TBF_]
      ,[aa_오른팔_부종_ECW__TBW_]
      ,[aa_왼_팔_부종_ECW__TBW_]
      ,[aa_몸통_부종_ECW__TBW_]
      ,[aa_오른다리_부종_ECW__TBW_]
      ,[a0_왼다리_부종_ECW__TBW_]
      ,[aa_전체_부종_ECW__TBW_]
      ,[aa_내장지방_단면적]
      ,[aa_비만도]
      ,[aa_체세포량]
      ,[aa_BMC]
      ,[aa_BMR]
      ,[aa_목둘레]
      ,[aa_가슴둘레]
      ,[aa_복부둘레]
      ,[a0_엉덩이_둘레]
      ,[aa_오른팔둘레]
      ,[aa_왼팔둘레]
      ,[aa_오른쪽_허벅지_둘레]
      ,[aa_왼쪽_허벅지_둘레]
      ,[aa_팔_근육_둘레]
      ,[aa_적정체중]
      ,[aa_체중조절량]
      ,[aa_체지방_조절량]
      ,[aa_근육_조절량]
      ,[a0_신체발달점수]
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
      ,[aa_akhz_RA__리액턴스]
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
      ,[aa_세포내_수분_표준범위_하한값]
      ,[aa_세포내_수분_표준범위_상한값]
      ,[aa_세포외_수분_표준범위_하한값]
      ,[aa_세포외_수분_표준범위_상한값]
      ,[a00_단백질_표준범위_하한값]
      ,[a0a_단백질_표준범위_상한값]
      ,[a0a_무기질_표준범위_하한값]
      ,[a0a_무기질_표준범위_상한값]
      ,[a0a_체지방량_표준범위_하한값]
      ,[a0a_체지방량_표준범위_상한값]
      ,[a0a_체중_표준범위_하한값]
      ,[a0a_체중_표준범위_상한값]
      ,[a0a_골격근량_표준범위_하한값]
      ,[a0a_골격근량_표준범위_상한값]
      ,[aa0_체지방량_표준범위_하한값]
      ,[aaa_체지방량_표준범위_상한값]
      ,[aaa_BMI_표준범위_하한값]
      ,[aaa_BMI표준범위_상한값]
      ,[aaa_체지방률_표준범위_하한값]
      ,[aaa_체지방률_표준범위_상한값]
      ,[aaa_WHR_표준범위_하한값]
      ,[aaa_WHR_표준범위_상한값]
      ,[aaa_비공개]
      ,[aaa_비공개1]
      ,[aa0_비공개]
      ,[aaa_비공개2]
      ,[aaa_비공개3]
      ,[aaa_비공개4]
      ,[aaa_비공개5]
      ,[aaa_비공개6]
      ,[aaa_비공개7]
      ,[aaa_비공개8]
      ,[aaa_비공개9]
      ,[aaa_비공개10]
      ,[aa0_비공개1]
      ,[aaa_비공개11]
      ,[aaa_비공개12]
      ,[aaa_비공개13]
      ,[aaa_비공개14]
      ,[aaa_비공개15]
      ,[aaa_비공개16]
      ,[aaa_오른팔_근육량]
      ,[aaa_왼팔근육량]
      ,[aaa_몸통_근육량]
      ,[aa0_오른_다리_근육량]
      ,[aaa_왼_다리_근육량]


 from 미병데이터final a inner join [dbo].[인바디$] b  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit

	  )v

	  
drop table #tmp17
select * into #tmp17 from (

select a.*, 
[자율신경_활성도]
      ,[자율신경_활성도_상태]
      ,[자율신경_균형도]
      ,[자율신경_균형도_상태]
      ,[스트레스_저항도]
      ,[스트레스_저항도_상태]
      ,[스트레스_지수]
      ,[스트레스_지수_상태]
      ,[피로도]
      ,[피로도_상태]
      ,[평균_심박동수]
      ,[평균_심박동수_상태]
      ,[심장_안정도]
      ,[심장_안정도_상태]
      ,[이상_심박동수]
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
	  on a.대상자_등록번호=b.대상자_등록번호 and a.Visit=b.Visit
)v

drop table #tmp18
select * into #tmp18 from (

select a.* ,[Wave_Type]
      ,[Vessel_Status_Score]
      ,[DPI(미분맥파지수)]
      ,[미분맥파지수_상태]
      ,[SP(박출강도)]
      ,[박출강도_상태]
      ,[BVT(혈관의_탄성도)]
      ,[혈관의_탄성도_상태]
      ,[RBV(잔혈량)]
      ,[잔혈량_상태]
      ,[VCT(수축시간)]
      ,[HR(심박수)]
      ,[심박수_상태] from #tmp17 a inner join  [dbo].[HRV_APGresult$] b 
	  on a.대상자_등록번호=b.대상자등록번호 and a.Visit=b.Visit
)v



drop table #tmp19
select * into #tmp19 from (

select a.* ,[SOS]
      ,[OI]
      ,[Tscore]
      ,[Zscore] from #tmp18 a inner join [dbo].[골밀도$] b 
	  on a.대상자_등록번호=b.대상자등록번호 and a.Visit=b.Visit
)v

drop table 미병데이터final



select * into [미병데이터final] from 
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
,Waist2heightd1 = 체중/ 신장

,BCM_BW=aa_체세포량/체중
,세포내액_세포외액=a_세포내액/a_세포외액
,미병분류fi=미병fg
,미병점수fi=미병점수 
,체질진단fi=체질진단

from #tmp19)v

select count(*),체질진단KS15 from [미병데이터final] 
WHERE VISIT='D1'
group by 체질진단KS15 

begin tran
update 미병데이터final set 미병fg = case when 미병그룹=1 then '건강군'
                                                     when 미병그룹=2 then '미병1' when 미병그룹=3 then '미병2' end
commit
sp_help 미병데이터final

SELECT * INTO #TMP FROM (

select A.*,B.KS15 AS 체질진단KS15 from 미병데이터final A LEFT JOIN [체질진단KS15$] B ON A.대상자_등록번호=B.[대상자 등록번호]

) A

drop table 미병데이터final

select * into 미병데이터final from #TMP

select 성별 from 미병데이터final group by 성별

-------0608일에 기기 테이블 loading 
select * from [dbo].[기기$]

drop table 미병데이터final_기기추가
select * into 미병데이터final_기기추가 from (
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
      ,[세포내액_세포외액]
      ,[미병분류fi]
      ,[미병점수fi]
      ,[체질진단fi] from 미병데이터final a inner join [기기$] b on a.대상자_등록번호=b.대상자등록번호 and a.Visit=b.Visit
)v


 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '미병데이터final_기기추가'
 and column_name like '%겨드%'

 
 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '미병데이터final'
 and column_name like '%겨드%'

select * from [dbo].['계측정보(활력_체표_악력_통각_체형)$']

select * into [dbo].[미병데이터final_선택지표2] from 
(
 select 
 a.Apen
 ,(a.aa_체세포량/체중) as BCM체중비
  ,a.a_근육량
 ,a.aa_내장지방_단면적
 ,a.a_단백질
 ,a.a_무기질
 ,a.aa_전체_부종_ECW__TBW_
,a.aa_전체_부종_ECF__TBF_
,a.a_제지방량
,a.aa_체세포량
,a.a_체수분
 ,a.a_체지방
,a.aa_체지방률
,a.a_뼈에있는_무기질량
,a.[세포내액_세포외액]
,a.a_세포내액
,a.aa_골격근량
,a.a_세포외액
 ,ECI,ECO___  as ECO_per,ecri, esi
,a.이상_심박동수
,(chestSized1/체중) as 겨드랑이둘레체중비
,(gokkolSized1/체중) as 곡골둘레체중비
,(neckSized1/체중) as 목둘레체중비
,(foreheadSized1/체중) as 이마체중비
,(악력_우1+악력_우2+악력_좌1+악력_좌2)/4 as 악력평균
,(체표_손바닥_좌1+체표_손바닥_좌2+체표_손바닥_우1+체표_손바닥_우2)/4 as 체표평균
,(통각_상완_좌1+통각_상완_좌2+통각_상완_우1+통각_상완_우2)/4 as 통각평균

,VO2_Peak
,METS_RC
,VE_LT
,VE_RC
,VE_Peak
,VT_LT
,VT_RC
,VT_Peak
,VO2_HR_LT비
,VO2_HR_RC비
,VO2_HR_Peak비
,VCO2_LT
,VCO2_RC
,VCO2_Peak
,VE_VCO2비
 
 ,성별,
a.[미병분류fi],
a.[미병점수fi],
a.[체질진단fi]

from 미병데이터final a inner join 미병데이터final_기기추가 b on a.[대상자_등록번호] =b.[대상자등록번호] and a.Visit=b.Visit
inner join  [대사량분석자료$] c on a.[대상자_등록번호] =c.[대상자 등록번호] and a.Visit=c.Visit
) v

select * from [dbo].['4-7data$']

select [대상자등록번호],[Visit],
from 미병데이터final_기기추가

select * from [대사량분석자료$]

select 
[대상자 등록번호],[Visit]
VO2_Peak
,METS_RC
,VE_LT
,VE_RC
,VE_Peak
,VT_LT
,VT_RC
,VT_Peak
,VO2_HR_LT비
,VO2_HR_RC비
,VO2_HR_Peak비
,VCO2_LT
,VCO2_RC
,VCO2_Peak
,VE_VCO2비
 from [dbo].[대사량분석자료$]




  

 select EQ5D_VAS from 미병데이터final
 select * from 미병데이터final_기기추가

 drop table 미병데이터final_선택지표 
 select * into 미병데이터final_선택지표 from (

 select  ECI,ECO___  ,ecri, esi,RMR_체중,[aa_골격근량],[a_단백질]
,[a_근육량]
,[aa_내장지방_단면적]
,[a_무기질]
,[a_뼈에있는_무기질량],b.*



 from 
 (
select [대상자등록번호],[Visit], Apen,aa_BMC,aa_BMR,ECI,ECO___ ,ecri, esi,RMR/a0_체중_ as RMR_체중,[aa_골격근량],[a_단백질]
,[a_근육량]
,[aa_내장지방_단면적]
,[a_무기질],
[a_뼈에있는_무기질량]

 from 미병데이터final_기기추가
) a inner join (
select
[대상자_등록번호],
[Visit]
 --,BDI_Score
--,cfs_score
--,[EQ5D_Values]
--,[GSRS_Score]
--,NEO_성실성
--,NEO_신경증
--,NEO_외향성
--,PSQI_Total
--,PWI_Socre
--,SF12_MCS
--,SF12_PCS
--,SMH_Score
--,건강상태점수
--,aa_겨드랑이둘레
--,기울,기허
,[aa_목둘레]/체중 as 목둘레_체중
--,EQ5D_VAS
,[세포내액_세포외액]
,a_세포내액
,이상_심박동수
,aa_전체_부종_ECW__TBW_
,aa_전체_부종_ECF__TBF_
,a_제지방량
,aa_체세포량
,a_체수분
--,체중
,a_체지방
,aa_체지방률
--,한
--,혈허
,(악력_우1+악력_우2+악력_좌1+악력_좌2)/4 as 악력평균
,(체표_손바닥_좌1+체표_손바닥_좌2+체표_손바닥_우1+체표_손바닥_우2)/4 as 체표평균
,(통각_상완_좌1+통각_상완_좌2+통각_상완_우1+통각_상완_우2)/4 as 통각평균
 ,성별,
[미병분류fi],
[미병점수fi],[체질진단fi]
 from 미병데이터final ) b on a.[대상자등록번호]=b.[대상자_등록번호] and a.[Visit]=b.Visit

 )v  


 select * from [미병데이터final]





 --선택된 지표 3

 
 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '미병데이터final_기기추가'
 and column_name like '%es%'

 
 SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '미병데이터final'
 and column_name like '%whr%'

drop table [미병데이터final_선택지표3]
select * into [dbo].[미병데이터final_선택지표3] from 
(
 select  a.[대상자_등록번호],
 수축기혈압
 ,이완기혈압
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
,(foreheadSized1/체중) as 이마체중비
,(neckSized1/체중) as 목체중비
,(armpitCircumferenced1/체중) as 겨드랑이둘레체중비
,(chestSized1/체중) as 가슴체중비
,(ribSized1/체중) as 늑골체중비
,(waistSized1/체중) as 허리체중비
,(iliacSized1/체중) as 장골체중비
,(gokkolSized1/체중) as 곡골체중비
,(악력_우1+악력_우2)/2 as 악력우
,(악력_좌1+악력_좌2)/2 as 악력좌
,(체표_손바닥_좌1+체표_손바닥_좌2)/2 as 체표_손바닥_좌
,(체표_손바닥_우1+체표_손바닥_우2)/2 as 체표_손바닥_우
,((체표_이마1+체표_이마2)/2-(체표_손바닥_좌1+체표_손바닥_좌2)/2) as 이마_손바닥_좌_차
,((체표_이마1+체표_이마2)/2-(체표_손바닥_우1+체표_손바닥_우2)/2) as 이마_손바닥_우_차
,(통각_상완_좌1+통각_상완_좌2)/2 as 통각_상완_좌
,(통각_상완_우1+통각_상완_우2)/2 as 통각_상완_우
,(통각_어깨_좌1+통각_어깨_좌2)/2 as 통각_어깨_좌

,a.a_세포내액
,a.a_세포외액
,a.[세포내액_세포외액] as 세포내액외액비
,a.a_체수분
 ,a.a_단백질
 ,a.a_근육량
 ,a.a_무기질
,a.a_제지방량
,a.a_뼈에있는_무기질량
,a.a0_체중_
,a.aa_골격근량
,a.aa_체세포량
 ,(a.aa_체세포량/체중) as BCM체중비
 ,a.aa_BMR
 ,a.aa_체지방률
 
 ,a.aa_전체_부종_ECW__TBW_
,a.aa_전체_부종_ECF__TBF_

 ,a.Apen
 ,a.[Lf/Hf] as lf_hf
 ,a.스트레스_저항도
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
,VO2_HR_LT비
,VO2_HR_RC비
,VO2_HR_Peak비
,VCO2_LT
,VCO2_RC
,VCO2_Peak
,VE_VCO2비
 ,성별,
a.[미병분류fi],
a.[미병점수fi],
a.[체질진단fi]

from 미병데이터final a inner join 미병데이터final_기기추가 b on a.[대상자_등록번호] =b.[대상자등록번호] and a.Visit=b.Visit
inner join  [대사량분석자료$] c on a.[대상자_등록번호] =c.[대상자 등록번호] and a.Visit=c.Visit
) v



--20150622 체질진단값이 변경되어 다시, 해보기..
select * into [미병데이터final_선택지표3_체질값변경] from (
select a.*,b.체질진단 from [미병데이터final_선택지표3] a inner join [dbo].[체질진단$] b on a.대상자_등록번호=b.[대상자 등록번호]
) v

alter table [미병데이터final_선택지표3_체질값변경] drop column 대상자_등록번호
alter table [미병데이터final_선택지표3_체질값변경] drop column 체질진단fi
sp_rename '미병데이터final_선택지표3_체질값변경.체질진단', '체질진단fi'




--20150622 체질진단값이 변경되어 다시, 해보기..

select * into [미병데이터final_선택지표4_체질값변경KS15] from (
select a.*,b.ks15 from [미병데이터final_선택지표3] a inner join [dbo].[체질진단KS15$] b on a.대상자_등록번호=b.[대상자 등록번호]
) v


alter table [미병데이터final_선택지표4_체질값변경KS15] drop column 대상자_등록번호
alter table [미병데이터final_선택지표4_체질값변경KS15] drop column 체질진단fi
sp_rename '미병데이터final_선택지표4_체질값변경KS15.KS15', '체질진단fi'




select 체질진단fi,count(*) from [미병데이터final_선택지표3_체질값변경]
group by 체질진단fi

select 체질진단fi,count(*) from [미병데이터final_선택지표4_체질값변경KS15]
group by 체질진단fi