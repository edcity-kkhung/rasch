# Stage 1: Build the application
FROM node:20-alpine as builder

# Set the working directory inside the container
WORKDIR /app

# Copy package.json and package-lock.json (or yarn.lock)
COPY package.json ./
COPY package-lock.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application files
COPY . .

# Build the Vite React application for production
RUN npm run build

# Stage 2: Serve the production build
FROM nginx:alpine

# Copy the built application from the builder stage to the Nginx server
COPY --from=builder /app/dist /usr/share/nginx/html

# Expose port 80, the default HTTP port for Nginx
EXPOSE 80

# Command to start Nginx
CMD ["nginx", "-g", "daemon off;"]