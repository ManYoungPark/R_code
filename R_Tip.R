################읽어버리기 쉬운 R tip########################

##데이터 컬럼 선택

colnms<-c("sub_ID","a_Weight","aa_TBW_TotalBodyWater_", "aa_ICW_IntracellularWater_")
mb_tmp1_1<-mb_tmp_data[,names(mb_tmp_data) %in% colnms]

##컬럼명 변경
names(mb_tmp1_1)[names(mb_tmp1_1) == 'aa_FFM%ofRightArm'] <- 'aa_FFM_per_ofRightArm'
#rename 도 있을꺼다..



##특정 문자가 들어간 변수명 모두 출력
grep("TBW",namestmp,value=TRUE)



##새로운 변수 계산해서 넣기.. 두가지 방법 within 과 transform 사용.
mb_tmp1_2 <- within(mb_tmp1_1, {
  pp_aa_TBW_TotalBodyWater_div_Weight <- aa_TBW_TotalBodyWater_ / a_Weight
})

mb_tmp1_2<-transform(mb_tmp1_1
                     ,pp_aa_ICW_IntracellularWater_div_aa_ECW_ExtracellularWater_=aa_ICW_IntracellularWater_/aa_ECW_ExtracellularWater_
                     ,mi_total_group3=factor(mi_total_group3))



#특정 조건에 맞는 컬럼 제거.. 예제 ) NA값이 100개 이상인거 제거
mb_tmp2_1<-mb_tmp2[!colSums(is.na(mb_tmp2))>100]

#rename은 plyr에도 있고, dplyr에도 있다. 에러가 날때는 둘중 하나를 detach(패키지 이름) 시켜야한다.
detach("package:dplyr", unload=TRUE)

