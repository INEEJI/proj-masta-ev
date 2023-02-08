# proj-masta-ev
MASTA EV

(배경)
  - 기존 MASTA 전기차 브레이크 시스템은 제동력(kg)을 바탕으로 전륜/후륜/사이드(주차) 브레이크에 대한 합격/불합격을 테스트 하고 있음.  
  - 기존 ABS 검차기기 시스템은 전륜 1회, 후륜 1회, 사이드 1회씩 기본적으로 총 3회 측정이 이뤄짐.
    - A : 사이드슬립 측정기(차축 정렬상태의 이상유무를 확인하는 장비)
    - B : 제동력 시험기(해당 차축의 좌우 제동력 및 편차를 측정하여 이상 유무를 확인하는 장비)
    - S : 속도계 시험기(검사 차량을 속도계 시험기에 진입시켜 실제 차량의 속도와 차량계기판의 속도를 비교하여 이상유무 확인
  - 만약, 전륜/후륜/사이드 각 1차 측정시 불합격이 발생되었다면, 2차 측정이 이루어진다.
  - ABS 테스트 결과는 MASTA 사에서 현재 엑셀파일로 작성되고 있으며, ABS 테스트 결과가 자동적으로 수집된다면 본 이상탐지 알고리즘 또한 자동화될 수 있음.
 
 (문제)
  - 설비 자체의 이상탐지
 
 (해결)
  1. MASTA사의 ABS_TEST 결과 파일로부터, 측정되었다고 기록된 시간을 바탕으로, 속도센서 DB에서 해당 시간에 맞춰 속도 데이터를 추출한다.
  2. MASTA사의 ABS_TEST 결과 파일로부터, 이상탐지 결과와 비교하기 위한 합격여부 변수를 정의한다.  
  
    종합_합격 ∩ 전륜_합격∩ 후륜_합격 ∩ 사이드_합격 : 합격여부 => '합격'  
    
    ￢(종합_합격 ∩ 전륜_합격∩ 후륜_합격∩ 사이드_합격) : 합격여부 => '불합격'  
    
    * '종합' 합격여부는 은 전륜,후륜 측정 시 제동력 합을 기준으로 판정된다.
    * '전륜/후륜/사이드' 합격여부는 1차 측정시 불합격일때, 2차 합격이면 합격으로 처리한다.(2차 측정시에도 불합격이면 불합격)
    
  3. 변화점 탐지 알고리즘 중 Segment-Neigborhood 방법을 적용한다. 이때, MASTA EV에서 제공한 ABS_TEST 결과 파일로부터 '측정횟수' 만큼의 브레이크 구간을 찾는다. 
  
  4. 만일, 브레이크 테스트 구간 밖에서, 브레이크 테스트 구간 내 최대값들의 최소값보다 큰 반등을 갖는다면 이를 이상으로 판별한다.  
    C=이상판별 기준치(threshold)는 '각 브레이크 구간별 최대값들 중 최소값'으로 설정한다. 즉, 브레이크 구간 밖에서 속도 >= C 이면 이상으로 판별한다.
  **단, MASTA 측에서 기록한 측정시점이 분 단위로 기록되어있으므로, 0.1초 단위로 기록되는 속도 데이터 추출 시 매칭이 간혹 잘못 이루어질 가능성이 있다. 또한, 속도 급상승-급하강이 덜 포착되어 브레이크 구간을 잘못 탐지하는 경우 이상탐지 알고리즘이 잘 작동되지 않을 수 있다.**
