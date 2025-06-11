FROM rocker/shiny

# install gsutil, unzip
RUN apt-get update && \
    apt-get install -y apt-transport-https ca-certificates gnupg curl unzip && \
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
    apt-get update && apt-get install -y google-cloud-cli
    # crcmod for faster downloads:
    # python3 -m pip install --no-cache-dir -U crcmod

RUN Rscript -e 'install.packages(c("easyr", "tidyr", "glue", "shinyjs", "data.table", "leaflet", "dplyr", "shinycssloaders", "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "scales", "qs2", "stringr", "magrittr", "easyr"))'

EXPOSE 3838

CMD gsutil cp gs://import-loud-galaxy/app.zip ./ && \ 
    unzip app.zip && \
    rm app.zip && \
    Rscript -e 'shiny::runApp("app", port = 3838, host = "0.0.0.0")'
