from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from pydantic import BaseModel


app = FastAPI()


origins = [ 'http://localhost'
          , 'http://localhost:8000'
          ]


app.add_middleware( CORSMiddleware
                  , allow_origins=origins
                  , allow_credentials=True
                  , allow_methods=['*']
                  , allow_headers=['*']
                  )


class Item(BaseModel):
    name: str
    price: float
    is_offer: bool | None = None


@app.get('/')
async def index():
    return FileResponse('index.html')


@app.get('/items')
async def get_items(skip: int = 0, limit: int = 10) -> list[Item]:
    fake_items_db = [ {'item_name': 'Foo'}
                    , {'item_name': 'Foo'}
                    , {'item_name': 'Foo'}
                    ]
    return fake_items_db[skip : skip + limit]


@app.get('/items/{item_id}')
async def get_item(item_id: int, q: str | None = None, short: bool = False):
    item = {'item_id': item_id}
    match (q, short):
        case (None, False):
            pass
        case (None, True):
            item.update({'description': 'This is an amazing item that has a long description'})
        case (_, False):
            item.update({'q': q})
        case (_, True):
            item.update({'q': q, 'description': 'This is an amazing item that has a long description'})
    return item



@app.put('/items/{item_id}')
async def put_item(item_id: int, item: Item):
    return {'item_id': item_id, 'item_name': item.name}

