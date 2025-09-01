"""
검사 결과 표시 UI 컴포넌트

문서 검사 결과를 카테고리별로 표시하고, 오류 세부사항 및 수정 제안을 제공하며,
우선순위별 문제점을 강조하여 표시하는 컴포넌트입니다.
"""

import streamlit as st
from typing import Dict, List, Any, Optional
import logging
from datetime import datetime
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots

logger = logging.getLogger(__name__)

class ResultDisplayComponent:
    """검사 결과 표시 UI 컴포넌트 클래스"""
    
    # 검사 유형별 아이콘 및 색상
    CHECK_TYPE_CONFIG = {
        'grammar': {
            'icon': '📝',
            'name': '구문 검사',
            'color': '#FF6B6B',
            'description': '한국어 문법 오류 검출'
        },
        'korean_spell': {
            'icon': '🔤',
            'name': '한국어 맞춤법',
            'color': '#4ECDC4',
            'description': '한국어 맞춤법 및 띄어쓰기 검사'
        },
        'english_spell': {
            'icon': '🔠',
            'name': '영어 맞춤법',
            'color': '#45B7D1',
            'description': '영어 단어 철자 검사'
        },
        'layout_consistency': {
            'icon': '📋',
            'name': '레이아웃 일관성',
            'color': '#96CEB4',
            'description': '제목 스타일 및 문서 형식 일관성'
        },
        'terminology_consistency': {
            'icon': '📚',
            'name': '용어 일관성',
            'color': '#FFEAA7',
            'description': '전문용어 사용 통일성'
        }
    }
    
    # 우선순위별 설정
    PRIORITY_CONFIG = {
        'high': {
            'icon': '🔴',
            'name': '높음',
            'color': '#FF4757',
            'background': '#FFE8E8'
        },
        'medium': {
            'icon': '🟡',
            'name': '보통',
            'color': '#FFA502',
            'background': '#FFF4E6'
        },
        'low': {
            'icon': '🟢',
            'name': '낮음',
            'color': '#2ED573',
            'background': '#E8F5E8'
        }
    }
    
    def __init__(self):
        """결과 표시 컴포넌트를 초기화합니다."""
        pass
    
    def render_results_header(self, results: Dict[str, Any]) -> None:
        """
        검사 결과 헤더를 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.header("📊 문서 검사 결과")
        
        # 기본 정보 표시
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.metric(
                label="📄 검사 파일",
                value=results.get('filename', 'Unknown'),
                help="검사된 문서 파일명"
            )
        
        with col2:
            total_errors = results.get('total_errors', 0)
            st.metric(
                label="⚠️ 총 문제점",
                value=f"{total_errors}개",
                help="발견된 전체 문제점 개수"
            )
        
        with col3:
            overall_score = results.get('overall_score', 0)
            score_color = "normal"
            if overall_score >= 80:
                score_color = "normal"
            elif overall_score >= 60:
                score_color = "normal"
            else:
                score_color = "inverse"
            
            st.metric(
                label="📈 품질 점수",
                value=f"{overall_score:.1f}/100",
                help="문서 전체 품질 점수"
            )
        
        st.divider()
    
    def render_priority_summary(self, results: Dict[str, Any]) -> None:
        """
        우선순위별 문제점 요약을 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        priority_issues = results.get('priority_issues', [])
        
        if not priority_issues:
            st.info("🎉 우선순위가 높은 문제점이 발견되지 않았습니다!")
            return
        
        st.subheader("🚨 우선 수정이 필요한 문제점")
        
        # 우선순위별로 그룹화
        priority_groups = {}
        for issue in priority_issues:
            priority = issue.get('priority', 'medium')
            if priority not in priority_groups:
                priority_groups[priority] = []
            priority_groups[priority].append(issue)
        
        # 우선순위 순서로 표시 (high -> medium -> low)
        for priority in ['high', 'medium', 'low']:
            if priority in priority_groups:
                self._render_priority_group(priority, priority_groups[priority])
    
    def _render_priority_group(self, priority: str, issues: List[Dict[str, Any]]) -> None:
        """
        특정 우선순위 그룹의 문제점들을 렌더링합니다.
        
        Args:
            priority: 우선순위 레벨
            issues: 해당 우선순위의 문제점 목록
        """
        config = self.PRIORITY_CONFIG.get(priority, self.PRIORITY_CONFIG['medium'])
        
        with st.expander(f"{config['icon']} {config['name']} 우선순위 ({len(issues)}개)", expanded=(priority == 'high')):
            for i, issue in enumerate(issues):
                self._render_priority_issue(issue, config, i + 1)
    
    def _render_priority_issue(self, issue: Dict[str, Any], config: Dict[str, str], index: int) -> None:
        """
        개별 우선순위 문제점을 렌더링합니다.
        
        Args:
            issue: 문제점 데이터
            config: 우선순위 설정
            index: 문제점 번호
        """
        # 문제점 컨테이너
        container = st.container()
        
        with container:
            # 문제점 헤더
            col1, col2 = st.columns([4, 1])
            
            with col1:
                st.markdown(f"**{index}. {issue.get('title', '문제점')}**")
                st.markdown(f"📍 위치: {issue.get('location', 'N/A')}")
            
            with col2:
                check_type = issue.get('check_type', 'unknown')
                type_config = self.CHECK_TYPE_CONFIG.get(check_type, {})
                if type_config:
                    st.markdown(f"{type_config.get('icon', '📝')} {type_config.get('name', check_type)}")
            
            # 문제점 내용
            if issue.get('current_text'):
                st.markdown(f"**현재:** `{issue['current_text']}`")
            
            if issue.get('suggested_text'):
                st.markdown(f"**제안:** `{issue['suggested_text']}`")
            
            if issue.get('explanation'):
                st.markdown(f"**설명:** {issue['explanation']}")
            
            # 신뢰도 표시
            confidence = issue.get('confidence', 0)
            if confidence > 0:
                st.progress(confidence / 100, text=f"신뢰도: {confidence}%")
            
            st.markdown("---")
    
    def render_category_results(self, results: Dict[str, Any]) -> None:
        """
        카테고리별 검사 결과를 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        check_results = results.get('check_results', [])
        
        if not check_results:
            st.warning("검사 결과가 없습니다.")
            return
        
        st.subheader("📋 카테고리별 검사 결과")
        
        # 탭으로 카테고리 분리
        tab_names = []
        tab_data = []
        
        for result in check_results:
            check_type = result.get('check_type', 'unknown')
            config = self.CHECK_TYPE_CONFIG.get(check_type, {})
            
            tab_name = f"{config.get('icon', '📝')} {config.get('name', check_type)}"
            tab_names.append(tab_name)
            tab_data.append(result)
        
        if tab_names:
            tabs = st.tabs(tab_names)
            
            for tab, result in zip(tabs, tab_data):
                with tab:
                    self._render_category_detail(result)
    
    def _render_category_detail(self, result: Dict[str, Any]) -> None:
        """
        개별 카테고리의 상세 결과를 렌더링합니다.
        
        Args:
            result: 카테고리별 검사 결과
        """
        check_type = result.get('check_type', 'unknown')
        config = self.CHECK_TYPE_CONFIG.get(check_type, {})
        
        # 카테고리 요약
        col1, col2 = st.columns(2)
        
        with col1:
            errors_found = result.get('errors_found', 0)
            st.metric(
                label="발견된 문제점",
                value=f"{errors_found}개"
            )
        
        with col2:
            if config.get('description'):
                st.info(config['description'])
        
        # 요약 정보
        summary = result.get('summary', '')
        if summary:
            st.markdown("**📝 요약**")
            st.markdown(summary)
            st.divider()
        
        # 상세 문제점 목록
        suggestions = result.get('suggestions', [])
        if suggestions:
            st.markdown("**🔍 발견된 문제점**")
            
            for i, suggestion in enumerate(suggestions):
                self._render_suggestion_item(suggestion, i + 1)
        else:
            st.success("✅ 이 카테고리에서는 문제점이 발견되지 않았습니다!")
    
    def _render_suggestion_item(self, suggestion: Dict[str, Any], index: int) -> None:
        """
        개별 수정 제안 항목을 렌더링합니다.
        
        Args:
            suggestion: 수정 제안 데이터
            index: 항목 번호
        """
        with st.expander(f"{index}. {suggestion.get('title', '수정 제안')}", expanded=False):
            # 위치 정보
            location = suggestion.get('location', '')
            if location:
                st.markdown(f"**📍 위치:** {location}")
            
            # 오류 유형
            error_type = suggestion.get('error_type', '')
            if error_type:
                st.markdown(f"**🏷️ 오류 유형:** {error_type}")
            
            # 현재 텍스트와 제안 텍스트
            current_text = suggestion.get('current_text', '')
            suggested_text = suggestion.get('suggested_text', '')
            
            if current_text or suggested_text:
                col1, col2 = st.columns(2)
                
                with col1:
                    if current_text:
                        st.markdown("**현재 텍스트:**")
                        st.code(current_text, language=None)
                
                with col2:
                    if suggested_text:
                        st.markdown("**수정 제안:**")
                        st.code(suggested_text, language=None)
            
            # 설명
            explanation = suggestion.get('explanation', '')
            if explanation:
                st.markdown("**💡 설명:**")
                st.markdown(explanation)
            
            # 신뢰도
            confidence = suggestion.get('confidence', 0)
            if confidence > 0:
                st.markdown("**🎯 신뢰도:**")
                st.progress(confidence / 100, text=f"{confidence}%")
    
    def render_recommendations(self, results: Dict[str, Any]) -> None:
        """
        전체 수정 권장사항을 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        recommendations = results.get('recommendations', [])
        
        if not recommendations:
            return
        
        st.subheader("💡 수정 권장사항")
        
        for i, recommendation in enumerate(recommendations):
            st.markdown(f"{i + 1}. {recommendation}")
    
    def render_comprehensive_dashboard(self, results: Dict[str, Any]) -> None:
        """
        종합 보고서 대시보드를 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.header("📊 종합 품질 대시보드")
        
        # 전체 통계 카드
        self._render_statistics_cards(results)
        
        # 시각화 차트
        col1, col2 = st.columns(2)
        
        with col1:
            self._render_category_chart(results)
        
        with col2:
            self._render_priority_chart(results)
        
        # 품질 점수 게이지
        self._render_quality_gauge(results)
        
        # 상세 통계 테이블
        self._render_detailed_statistics(results)
        
        # 트렌드 분석 (향후 확장용)
        self._render_trend_analysis(results)
    
    def _render_statistics_cards(self, results: Dict[str, Any]) -> None:
        """
        통계 카드들을 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        check_results = results.get('check_results', [])
        total_errors = results.get('total_errors', 0)
        overall_score = results.get('overall_score', 0)
        
        # 카테고리별 통계 계산
        category_stats = {}
        total_suggestions = 0
        
        for result in check_results:
            check_type = result.get('check_type', 'unknown')
            errors_found = result.get('errors_found', 0)
            suggestions_count = len(result.get('suggestions', []))
            
            category_stats[check_type] = {
                'errors': errors_found,
                'suggestions': suggestions_count
            }
            total_suggestions += suggestions_count
        
        # 통계 카드 표시
        col1, col2, col3, col4, col5 = st.columns(5)
        
        with col1:
            st.metric(
                label="📄 검사 완료",
                value="✅",
                help="문서 검사가 성공적으로 완료됨"
            )
        
        with col2:
            st.metric(
                label="⚠️ 총 문제점",
                value=f"{total_errors}개",
                help="발견된 전체 문제점 개수"
            )
        
        with col3:
            st.metric(
                label="💡 수정 제안",
                value=f"{total_suggestions}개",
                help="제공된 수정 제안 개수"
            )
        
        with col4:
            st.metric(
                label="📈 품질 점수",
                value=f"{overall_score:.1f}",
                delta=f"{overall_score - 70:.1f}" if overall_score != 0 else None,
                help="문서 전체 품질 점수 (기준: 70점)"
            )
        
        with col5:
            # 검사 카테고리 수
            categories_checked = len([r for r in check_results if r.get('errors_found', 0) >= 0])
            st.metric(
                label="🔍 검사 항목",
                value=f"{categories_checked}개",
                help="수행된 검사 카테고리 수"
            )
    
    def _render_category_chart(self, results: Dict[str, Any]) -> None:
        """
        카테고리별 문제점 차트를 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.subheader("📊 카테고리별 문제점 분포")
        
        check_results = results.get('check_results', [])
        
        if not check_results:
            st.info("표시할 데이터가 없습니다.")
            return
        
        # 데이터 준비
        categories = []
        error_counts = []
        colors = []
        
        for result in check_results:
            check_type = result.get('check_type', 'unknown')
            config = self.CHECK_TYPE_CONFIG.get(check_type, {})
            
            categories.append(config.get('name', check_type))
            error_counts.append(result.get('errors_found', 0))
            colors.append(config.get('color', '#95A5A6'))
        
        # 파이 차트 생성
        if sum(error_counts) > 0:
            fig = px.pie(
                values=error_counts,
                names=categories,
                color_discrete_sequence=colors,
                title="카테고리별 문제점 분포"
            )
            fig.update_traces(textposition='inside', textinfo='percent+label')
            fig.update_layout(height=400)
            st.plotly_chart(fig, use_container_width=True)
        else:
            st.success("🎉 모든 카테고리에서 문제점이 발견되지 않았습니다!")
    
    def _render_priority_chart(self, results: Dict[str, Any]) -> None:
        """
        우선순위별 문제점 차트를 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.subheader("🚨 우선순위별 문제점")
        
        priority_issues = results.get('priority_issues', [])
        
        if not priority_issues:
            st.success("🎉 우선순위가 높은 문제점이 없습니다!")
            return
        
        # 우선순위별 카운트
        priority_counts = {'high': 0, 'medium': 0, 'low': 0}
        
        for issue in priority_issues:
            priority = issue.get('priority', 'medium')
            if priority in priority_counts:
                priority_counts[priority] += 1
        
        # 바 차트 데이터 준비
        priorities = []
        counts = []
        colors = []
        
        for priority, count in priority_counts.items():
            if count > 0:
                config = self.PRIORITY_CONFIG.get(priority, {})
                priorities.append(f"{config.get('icon', '')} {config.get('name', priority)}")
                counts.append(count)
                colors.append(config.get('color', '#95A5A6'))
        
        if counts:
            # 바 차트 생성
            fig = px.bar(
                x=priorities,
                y=counts,
                color=priorities,
                color_discrete_sequence=colors,
                title="우선순위별 문제점 개수"
            )
            fig.update_layout(
                showlegend=False,
                height=400,
                xaxis_title="우선순위",
                yaxis_title="문제점 개수"
            )
            st.plotly_chart(fig, use_container_width=True)
    
    def _render_quality_gauge(self, results: Dict[str, Any]) -> None:
        """
        품질 점수 게이지를 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.subheader("📈 문서 품질 점수")
        
        overall_score = results.get('overall_score', 0)
        
        # 게이지 차트 생성
        fig = go.Figure(go.Indicator(
            mode = "gauge+number+delta",
            value = overall_score,
            domain = {'x': [0, 1], 'y': [0, 1]},
            title = {'text': "품질 점수"},
            delta = {'reference': 70, 'increasing': {'color': "green"}, 'decreasing': {'color': "red"}},
            gauge = {
                'axis': {'range': [None, 100]},
                'bar': {'color': "darkblue"},
                'steps': [
                    {'range': [0, 50], 'color': "lightgray"},
                    {'range': [50, 70], 'color': "yellow"},
                    {'range': [70, 90], 'color': "lightgreen"},
                    {'range': [90, 100], 'color': "green"}
                ],
                'threshold': {
                    'line': {'color': "red", 'width': 4},
                    'thickness': 0.75,
                    'value': 90
                }
            }
        ))
        
        fig.update_layout(height=300)
        st.plotly_chart(fig, use_container_width=True)
        
        # 점수 해석
        if overall_score >= 90:
            st.success("🌟 우수한 품질의 문서입니다!")
        elif overall_score >= 70:
            st.info("👍 양호한 품질의 문서입니다.")
        elif overall_score >= 50:
            st.warning("⚠️ 개선이 필요한 문서입니다.")
        else:
            st.error("🔴 많은 수정이 필요한 문서입니다.")
    
    def _render_detailed_statistics(self, results: Dict[str, Any]) -> None:
        """
        상세 통계 테이블을 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.subheader("📋 상세 통계")
        
        check_results = results.get('check_results', [])
        
        if not check_results:
            st.info("표시할 통계가 없습니다.")
            return
        
        # 테이블 데이터 준비
        table_data = []
        
        for result in check_results:
            check_type = result.get('check_type', 'unknown')
            config = self.CHECK_TYPE_CONFIG.get(check_type, {})
            
            row = {
                '검사 항목': f"{config.get('icon', '📝')} {config.get('name', check_type)}",
                '발견된 문제점': result.get('errors_found', 0),
                '수정 제안': len(result.get('suggestions', [])),
                '상태': '✅ 완료' if result.get('errors_found', 0) >= 0 else '❌ 실패'
            }
            table_data.append(row)
        
        # 총계 행 추가
        total_errors = sum(row['발견된 문제점'] for row in table_data)
        total_suggestions = sum(row['수정 제안'] for row in table_data)
        
        table_data.append({
            '검사 항목': '**📊 총계**',
            '발견된 문제점': f"**{total_errors}**",
            '수정 제안': f"**{total_suggestions}**",
            '상태': '**✅ 완료**'
        })
        
        # 테이블 표시
        df = pd.DataFrame(table_data)
        st.dataframe(df, use_container_width=True, hide_index=True)
    
    def _render_trend_analysis(self, results: Dict[str, Any]) -> None:
        """
        트렌드 분석을 렌더링합니다 (향후 확장용).
        
        Args:
            results: 검사 결과 데이터
        """
        st.subheader("📈 분석 인사이트")
        
        # 현재는 기본적인 인사이트만 제공
        total_errors = results.get('total_errors', 0)
        overall_score = results.get('overall_score', 0)
        check_results = results.get('check_results', [])
        
        insights = []
        
        # 가장 문제가 많은 카테고리 찾기
        if check_results:
            max_errors = 0
            problem_category = None
            
            for result in check_results:
                errors = result.get('errors_found', 0)
                if errors > max_errors:
                    max_errors = errors
                    problem_category = result.get('check_type')
            
            if problem_category and max_errors > 0:
                config = self.CHECK_TYPE_CONFIG.get(problem_category, {})
                insights.append(f"🔍 **{config.get('name', problem_category)}** 영역에서 가장 많은 문제점({max_errors}개)이 발견되었습니다.")
        
        # 점수 기반 인사이트
        if overall_score >= 80:
            insights.append("✨ 전반적으로 높은 품질의 문서입니다. 소소한 개선사항만 적용하면 됩니다.")
        elif overall_score >= 60:
            insights.append("📝 문서 품질이 양호합니다. 주요 문제점들을 수정하면 더욱 향상될 것입니다.")
        else:
            insights.append("⚠️ 문서 품질 개선이 필요합니다. 우선순위가 높은 문제점부터 차례로 수정해보세요.")
        
        # 권장사항 기반 인사이트
        recommendations = results.get('recommendations', [])
        if len(recommendations) > 3:
            insights.append(f"💡 총 {len(recommendations)}개의 개선 권장사항이 있습니다. 단계적으로 적용해보세요.")
        
        # 인사이트 표시
        if insights:
            for insight in insights:
                st.info(insight)
        else:
            st.info("🎉 특별한 문제점이 발견되지 않은 우수한 문서입니다!")
    
    def render_export_options(self, results: Dict[str, Any]) -> None:
        """
        결과 내보내기 옵션을 렌더링합니다.
        
        Args:
            results: 검사 결과 데이터
        """
        st.subheader("📤 결과 내보내기")
        
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("📄 텍스트로 저장", help="검사 결과를 텍스트 파일로 저장"):
                text_content = self._generate_text_report(results)
                st.download_button(
                    label="💾 텍스트 파일 다운로드",
                    data=text_content,
                    file_name=f"검사결과_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt",
                    mime="text/plain"
                )
        
        with col2:
            if st.button("📊 요약 보고서", help="요약된 보고서 생성"):
                summary_content = self._generate_summary_report(results)
                st.download_button(
                    label="💾 요약 보고서 다운로드",
                    data=summary_content,
                    file_name=f"요약보고서_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt",
                    mime="text/plain"
                )
        
        with col3:
            if st.button("🔄 다시 검사", help="같은 파일을 다시 검사"):
                # 세션 상태를 검사 단계로 변경
                st.session_state.current_step = 'checking'
                st.session_state.check_results = None
                st.rerun()
    
    def _generate_text_report(self, results: Dict[str, Any]) -> str:
        """
        텍스트 형태의 상세 보고서를 생성합니다.
        
        Args:
            results: 검사 결과 데이터
            
        Returns:
            텍스트 보고서 내용
        """
        lines = []
        lines.append("=" * 60)
        lines.append("한국어 문서 검사 결과 보고서")
        lines.append("=" * 60)
        lines.append("")
        
        # 기본 정보
        lines.append(f"파일명: {results.get('filename', 'Unknown')}")
        lines.append(f"검사 시간: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        lines.append(f"총 문제점: {results.get('total_errors', 0)}개")
        lines.append(f"품질 점수: {results.get('overall_score', 0):.1f}/100")
        lines.append("")
        
        # 우선순위 문제점
        priority_issues = results.get('priority_issues', [])
        if priority_issues:
            lines.append("우선 수정이 필요한 문제점:")
            lines.append("-" * 40)
            for i, issue in enumerate(priority_issues):
                lines.append(f"{i + 1}. {issue.get('title', '문제점')}")
                lines.append(f"   위치: {issue.get('location', 'N/A')}")
                lines.append(f"   현재: {issue.get('current_text', 'N/A')}")
                lines.append(f"   제안: {issue.get('suggested_text', 'N/A')}")
                lines.append("")
        
        # 카테고리별 결과
        check_results = results.get('check_results', [])
        for result in check_results:
            check_type = result.get('check_type', 'unknown')
            config = self.CHECK_TYPE_CONFIG.get(check_type, {})
            
            lines.append(f"{config.get('name', check_type)} 검사 결과:")
            lines.append("-" * 40)
            lines.append(f"발견된 문제점: {result.get('errors_found', 0)}개")
            
            summary = result.get('summary', '')
            if summary:
                lines.append(f"요약: {summary}")
            
            lines.append("")
        
        # 권장사항
        recommendations = results.get('recommendations', [])
        if recommendations:
            lines.append("수정 권장사항:")
            lines.append("-" * 40)
            for i, rec in enumerate(recommendations):
                lines.append(f"{i + 1}. {rec}")
            lines.append("")
        
        return "\n".join(lines)
    
    def _generate_summary_report(self, results: Dict[str, Any]) -> str:
        """
        요약 보고서를 생성합니다.
        
        Args:
            results: 검사 결과 데이터
            
        Returns:
            요약 보고서 내용
        """
        lines = []
        lines.append("한국어 문서 검사 요약 보고서")
        lines.append("=" * 40)
        lines.append("")
        
        lines.append(f"📄 파일: {results.get('filename', 'Unknown')}")
        lines.append(f"⚠️ 총 문제점: {results.get('total_errors', 0)}개")
        lines.append(f"📈 품질 점수: {results.get('overall_score', 0):.1f}/100")
        lines.append("")
        
        # 카테고리별 요약
        check_results = results.get('check_results', [])
        if check_results:
            lines.append("카테고리별 문제점:")
            for result in check_results:
                check_type = result.get('check_type', 'unknown')
                config = self.CHECK_TYPE_CONFIG.get(check_type, {})
                errors = result.get('errors_found', 0)
                lines.append(f"• {config.get('name', check_type)}: {errors}개")
            lines.append("")
        
        # 주요 권장사항 (상위 3개)
        recommendations = results.get('recommendations', [])
        if recommendations:
            lines.append("주요 권장사항:")
            for i, rec in enumerate(recommendations[:3]):
                lines.append(f"{i + 1}. {rec}")
        
        return "\n".join(lines)

def render_check_results(results: Dict[str, Any]) -> None:
    """
    검사 결과를 렌더링하는 편의 함수
    
    Args:
        results: 검사 결과 데이터
    """
    if not results:
        st.error("검사 결과가 없습니다.")
        return
    
    component = ResultDisplayComponent()
    
    # 탭으로 보기 방식 선택
    tab1, tab2 = st.tabs(["📊 종합 대시보드", "📋 상세 결과"])
    
    with tab1:
        # 종합 보고서 대시보드
        component.render_comprehensive_dashboard(results)
    
    with tab2:
        # 기존 상세 결과 표시
        # 결과 헤더
        component.render_results_header(results)
        
        # 우선순위 요약
        component.render_priority_summary(results)
        
        # 카테고리별 결과
        component.render_category_results(results)
        
        # 권장사항
        component.render_recommendations(results)
    
    # 공통 내보내기 옵션
    st.divider()
    component.render_export_options(results)

def render_comprehensive_report(results: Dict[str, Any]) -> None:
    """
    종합 보고서만을 렌더링하는 함수
    
    Args:
        results: 검사 결과 데이터
    """
    if not results:
        st.error("검사 결과가 없습니다.")
        return
    
    component = ResultDisplayComponent()
    component.render_comprehensive_dashboard(results)