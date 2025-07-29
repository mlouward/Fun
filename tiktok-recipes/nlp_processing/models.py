import os

import dotenv
from sqlalchemy import ForeignKey, Integer, String, Text
from sqlalchemy.ext.asyncio import (AsyncAttrs, async_sessionmaker,
                                    create_async_engine)
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship


class Base(AsyncAttrs, DeclarativeBase):
    pass


class User(Base):
    __tablename__ = "users"
    id: Mapped[int] = mapped_column(primary_key=True)
    username: Mapped[str] = mapped_column(String(64), unique=True, nullable=False)
    hashed_password: Mapped[str] = mapped_column(String(128), nullable=False)
    recipes = relationship("Recipe", back_populates="user")


class Recipe(Base):
    __tablename__ = "recipes"
    id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[int] = mapped_column(ForeignKey("users.id"))
    title: Mapped[str] = mapped_column(String(256))
    servings: Mapped[int] = mapped_column(Integer)
    prep_time: Mapped[int] = mapped_column(Integer)
    cook_time: Mapped[int] = mapped_column(Integer)
    ingredients: Mapped[str] = mapped_column(Text)
    instructions: Mapped[str] = mapped_column(Text)
    cover_image_idx: Mapped[int] = mapped_column(Integer)
    tiktok_username: Mapped[str] = mapped_column(String(64), nullable=True)
    tiktok_video_id: Mapped[str] = mapped_column(String(32), nullable=True)
    user = relationship("User", back_populates="recipes")


# Database setup
# Get from .env file

dotenv.load_dotenv()
DATABASE_URL = os.environ.get("DATABASE_URL")
if not DATABASE_URL:
    raise ValueError("DATABASE_URL environment variable is not set")
engine = create_async_engine(DATABASE_URL, echo=True)
AsyncSessionLocal = async_sessionmaker(engine, expire_on_commit=False)
