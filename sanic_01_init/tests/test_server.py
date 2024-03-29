import pytest
from sanic import Sanic, response

@pytest.fixture
def app():
    sanic_app = Sanic("sanic-01-init-app")

    @sanic_app.get("/")
    def basic(request):
        return response.text("foo")

    return sanic_app


@pytest.mark.asyncio
async def test_basic_asgi_client(app):
    request, response = await app.asgi_client.get("/")

    assert request.method.lower() == "get"
    assert response.body == b"bar"
    assert response.status == 200
