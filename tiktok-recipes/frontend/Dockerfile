# Use official Node.js image for building
FROM node:20-alpine as build
WORKDIR /app
COPY . .
RUN yarn install --frozen-lockfile || npm install
RUN yarn build || npm run build

# Use a lightweight web server for static files
FROM nginx:alpine as production
WORKDIR /usr/share/nginx/html
COPY --from=build /app/dist .
COPY ./nginx.conf /etc/nginx/conf.d/default.conf
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
