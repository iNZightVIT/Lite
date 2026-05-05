FROM node:20-slim

WORKDIR /app

COPY package.json package-lock.json* ./
RUN npm install --omit=dev

COPY index.js ./
COPY www ./www

ENV PORT=8787
EXPOSE 8787

CMD ["node", "index.js"]
