#!/usr/bin/env python
import argparse
import sys
import os
from pathlib import Path
from enhanced_app import generate_business_report

def main():
    # 인자 파서 설정
    parser = argparse.ArgumentParser(
        description='CrewAI를 이용한 멀티에이전트 협업 시스템으로 비즈니스 보고서 생성',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
예시:
  python cli.py --topic "분기별 영업 실적" --output "my_reports"
  python cli.py -t "시장 동향 분석" -o "reports"
  python cli.py --topic "고객 만족도 조사" --data "data/survey.csv" "data/feedback.csv"
        '''
    )
    
    # 인자 추가
    parser.add_argument('--topic', '-t', type=str, required=True,
                        help='비즈니스 보고서의 주제')
    parser.add_argument('--data', '-d', type=str, nargs='+',
                        help='분석할 데이터 소스 파일 경로 (선택 사항, 제공하지 않으면 샘플 데이터 생성)')
    parser.add_argument('--output', '-o', type=str, default='output',
                        help='출력 파일을 저장할 디렉토리 (기본값: output)')
    
    # 인자가 없으면 도움말 표시
    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)
    
    # 인자 파싱
    args = parser.parse_args()
    
    # 출력 디렉토리 생성
    os.makedirs(args.output, exist_ok=True)
    
    # 보고서 생성
    print(f"\n[CrewAI 멀티에이전트 시스템] '{args.topic}' 주제로 비즈니스 보고서 생성을 시작합니다.\n")
    
    try:
        output_file = generate_business_report(args.topic, args.data, args.output)
        if output_file:
            print(f"\n✅ 성공: PowerPoint 보고서가 생성되었습니다: {output_file}\n")
            print(f"다음 명령어로 보고서를 열 수 있습니다:\n$ open {output_file}\n")
        else:
            print("\n❌ 오류: 보고서 생성에 실패했습니다.\n")
    except Exception as e:
        print(f"\n❌ 오류 발생: {str(e)}\n")
        sys.exit(1)

if __name__ == "__main__":
    main()
