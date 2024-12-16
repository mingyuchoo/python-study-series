import time
from sanic import Sanic
from sanic.response import HTTPResponse, json, text
from sanic.request import Request
from sanic_ext import Extend


app = Sanic("sanic-01-init-app")
Extend(app)


@app.get('/')
async def get_root(request: Request):
    return json({"seq": ["one", "two"]})


@app.get('/foo')
async def get_foo_handler(request):
    return json({'foo': 'I said foo!'})


@app.get('/sync')
def get_sync_handler(request):
    time.sleep(0.1)
    return text('Done.')


@app.get('/async')
async def get_async_handler(request):
    await asyncio.sleep(0.1)
    return text('Done.')


@app.get('/typed')
async def get_typed_handler(request: Request) -> HTTPResponse:
    return text('Done.')


@app.route('/tag/<tag:str>')
async def tag_handler(request, tag: str):
    return text("Tag - {}".format(tag))


if __name__ == '__main__':
    app.run()
