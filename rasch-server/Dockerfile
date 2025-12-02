# Base image: Node.js
FROM node:18

# Install R and dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    gdebi-core \
    wget \
    && apt-get clean

# Install RStudio Server
RUN wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2024.09.0-375-amd64.deb && \
    gdebi --non-interactive rstudio-server-2024.09.0-375-amd64.deb && \
    rm rstudio-server-2024.09.0-375-amd64.deb

# Create RStudio user
RUN useradd -m rstudio && echo "rstudio:rstudio" | chpasswd

# Install common R packages
# RUN R -e "install.packages(c('eRm','jsonlite', 'mirt'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('eRm','jsonlite'), repos='https://cloud.r-project.org')"

# Set working directory for Node.js app
WORKDIR /app

# Copy Node.js files
COPY package*.json ./
RUN npm install
COPY . .

# Expose ports: Node.js (3000) and RStudio Server (8787)
EXPOSE 3000 8787

# Start both services
CMD service rstudio-server start && npm start