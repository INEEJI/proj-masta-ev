# proj-masta-ev

## (배경)
  - 기존 MASTA 전기차 브레이크 시스템은 제동력(kg)을 바탕으로 전륜/후륜/사이드(주차) 브레이크에 대한 합격/불합격을 테스트 하고 있음.  
  - 기존 ABS 검차기기 시스템은 전륜 1회, 후륜 1회, 사이드 1회씩 기본적으로 총 3회 측정이 이뤄짐.
    - A : 사이드슬립 측정기(차축 정렬상태의 이상유무를 확인하는 장비)
    - B : 제동력 시험기(해당 차축의 좌우 제동력 및 편차를 측정하여 이상 유무를 확인하는 장비)
    - S : 속도계 시험기(검사 차량을 속도계 시험기에 진입시켜 실제 차량의 속도와 차량계기판의 속도를 비교하여 이상유무 확인)
  - 만약, 전륜/후륜/사이드 부분에서 1차 측정시 불합격이 발생되었다면, 해당 부분에 2차 측정이 이루어진다.
  - ABS 테스트 결과는 MASTA 사에서 현재 엑셀파일로 작성되고 있으며, ABS 테스트 결과가 자동적으로 수집된다면 본 이상탐지 알고리즘 또한 자동화될 수 있음.  


## (과제 목표)
  - 브레이크 이상으로 판별된 차량이 없을 때, 현 설비 자체의 이상탐지를 시도



## (제안 솔루션)
  1. MASTA사의 ABS_TEST 결과 파일로부터, 측정되었다고 기록된 시간을 바탕으로, 속도센서 DB에서 해당 시간에 맞춰 속도 데이터를 추출한다.
  2. MASTA사의 ABS_TEST 결과 파일로부터, 이상탐지 결과와 비교하기 위한 합격여부 변수를 정의한다.  
  
    종합_합격 ∩ 전륜_합격∩ 후륜_합격 ∩ 사이드_합격 : 합격여부 => '합격'  
    
    ￢(종합_합격 ∩ 전륜_합격∩ 후륜_합격∩ 사이드_합격) : 합격여부 => '불합격'  
    
    * '종합' 합격여부는 은 전륜,후륜 측정 시 제동력 합을 기준으로 판정된다.
    * '전륜/후륜/사이드' 합격여부는 1차 측정시 불합격일때, 2차 합격이면 합격으로 처리한다.(2차 측정시에도 불합격이면 불합격)
    
  3. 변화점 탐지 알고리즘 중 Segment-Neigborhood 방법을 적용한다. 이때, MASTA EV에서 제공한 ABS_TEST 결과 파일로부터 '측정횟수' 만큼의 브레이크 구간을 찾는다. 
  
  4. 만일, 브레이크 테스트 구간 밖에서, 브레이크 테스트 구간 내 최대값들의 최소값보다 큰 반등을 갖는다면 이를 이상으로 판별한다.  
    **이상판별 기준값(threshold) C는 '각 브레이크 구간별 최대값들 중 최소값'으로 설정한다. 즉, 브레이크 구간 밖에서 속도 >= C 이면 이상(anomaly)으로 판별한다.**
    

  5. **이상탐지 알고리즘이 기존 abs테스트 결과와 부합하게 잘 작동했을 때, abs 테스트에서 합격여부='합격'으로 처리된 차량 중 이상탐지 알고리즘에서 이상이 발견된다면 해당 차량/현 설비에 문제가 있는 것으로 검토할 것을 제안.**
  


## (평가)
 MASTA에서 기록한 1월 데이터 100건의 테스트 정보를 통해, 이상탐지결과 변수와 합격여부 변수를 비교한 F1-Score 결과
  
| 속도변수 | F1-Score |
| --- | --- |
| X | 0.523 |
| Y | 0.703 |
| Z | 0.744 |
| PC1 | 0.644 |

- X, Y, Z 변수 중 Z 변수가 가장 속도 변화를 잘 포착. YZ 회전축을 동시에 고려하거나, XYZ변수를 주성분 변환했을 때보다 Z 단일변수를 이용한 이상탐지 정확도가 가장 좋게 나타났음.
  
    
    
### (문제점 및 보완계획)
**현재, MASTA에서는 ABS 테스트 결과를 제공하는 과정에서 차량별 테스트 시작시간, 종료시간을 '분' 단위로 기록하여 제공하고있다.**

![image](https://user-images.githubusercontent.com/124751879/217473140-a6725070-4854-46c8-92cb-a58cd1231bf1.png)

- **하지만 위와 같은 경우에는 종료시간과 시작시간이 겹쳐 속도센서 DB로부터 데이터를 추출하는 과정에서 중첩되는 영역이 생긴다.**
- 위 문제로 인해 측정횟수에 따라 브레이크 구간을 포착하는 이상탐지 알고리즘 수행에 문제가 생긴다.
- 현 이슈를 해결하기 위해, 2월 8일부로 이전차량의 종료시간과 현재차량의 시작시간이 최소 1분의 대기시간을 두고 측정 및 기록될 수 있도록 협조를 구해놓은 상황이며 해당 이슈가 해결된 2월 데이터 100건을 수집요청을 해놓은 상황이며, 2월 데이터 수집이 끝난 후 더 보완된 성능평가 결과치를 제시할 예정이다.

 
##  (알고리즘 작동 예시)  
  
  ![ezgif-1-8e70d052db](https://user-images.githubusercontent.com/124751879/217446105-8c223d7e-fe28-4a76-bed0-b0fc287244a8.gif)

