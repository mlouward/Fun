# User authentication routes and logic
import os

from fastapi import APIRouter, Depends, HTTPException
from jose import jwt
from passlib.context import CryptContext
from pydantic import BaseModel
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession

from .models import AsyncSessionLocal, User

SECRET_KEY = os.environ.get("SECRET_KEY", "dev-secret-key")
ALGORITHM = "HS256"
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

router = APIRouter(tags=["auth"])


class UserCreate(BaseModel):
    username: str
    password: str


class UserLogin(BaseModel):
    username: str
    password: str


async def get_db():
    async with AsyncSessionLocal() as session:
        yield session


@router.post("/register")
async def register(user: UserCreate, db: AsyncSession = Depends(get_db)) -> dict[str, str]:
    result = await db.execute(select(User).where(User.username == user.username))
    if result.scalar():
        raise HTTPException(status_code=400, detail="Username already registered")
    hashed = pwd_context.hash(user.password)
    db_user = User(username=user.username, hashed_password=hashed)
    db.add(db_user)
    await db.commit()
    return {"message": "User registered successfully"}


@router.post("/login")
async def login(user: UserLogin, db: AsyncSession = Depends(get_db)) -> dict[str, str]:
    result = await db.execute(select(User).where(User.username == user.username))
    db_user = result.scalar()
    if not db_user or not pwd_context.verify(user.password, db_user.hashed_password):
        raise HTTPException(status_code=401, detail="Invalid credentials")
    token = jwt.encode({"sub": db_user.username}, SECRET_KEY, algorithm=ALGORITHM)
    return {"access_token": token, "token_type": "bearer"}
