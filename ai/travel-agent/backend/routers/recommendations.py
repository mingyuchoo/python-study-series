from fastapi import APIRouter, HTTPException
from backend.models.request_models import RecommendationRequest, SearchRequest
from backend.models.response_models import RecommendationResponse, APIResponse, TravelPackage
from backend.services.recommendation_service import recommendation_service

router = APIRouter(prefix="/recommendations", tags=["Recommendations"])

@router.post("/", response_model=RecommendationResponse)
async def get_recommendations(rec_request: RecommendationRequest):
    """개인화된 여행 상품 추천"""
    try:
        recommendations = recommendation_service.get_recommendations(
            rec_request.session_id, 
            rec_request.limit
        )
        
        return RecommendationResponse(
            session_id=rec_request.session_id,
            recommendations=recommendations,
            total_count=len(recommendations)
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/search")
async def search_packages(search_request: SearchRequest):
    """여행 상품 검색"""
    try:
        # 검색 로직 구현
        user_preferences = {
            "destination": search_request.query,
            "interests": [search_request.query]
        }
        
        if search_request.filters:
            user_preferences.update(search_request.filters)
        
        packages = recommendation_service.vector_store.search_travel_packages(
            user_preferences, 
            search_request.limit
        )
        
        travel_packages = []
        for package in packages:
            travel_packages.append(TravelPackage(
                package_id=package['package_id'],
                package_name=package['package_name'],
                country=package['country'],
                city=package['city'],
                duration_days=package['duration_days'],
                min_price=package['min_price'],
                max_price=package['max_price'],
                package_type=package['metadata'].get('package_type', ''),
                description=package['metadata'].get('description', ''),
                highlights=package['metadata'].get('highlights', ''),
                similarity_score=package['similarity_score']
            ))
        
        return APIResponse(
            success=True, 
            message="Search completed", 
            data={
                "packages": travel_packages,
                "total_count": len(travel_packages)
            }
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
