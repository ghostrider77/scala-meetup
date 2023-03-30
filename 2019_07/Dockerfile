FROM python:3.9-slim

WORKDIR /src

COPY . .

RUN apt-get update && \
    apt-get -y install curl openjdk-11-jdk && \
    pip install -r requirements.txt && \
    curl -Lo coursier https://git.io/coursier-cli && \
    chmod +x coursier && \
    ./coursier launch almond:0.13.4 --scala 2.12.9 -- --install --id scala212 --display-name "Scala 2.12" && \
    rm -f coursier

CMD ["jupyter", "notebook", "--port=8080", "--no-browser", "--ip=0.0.0.0", "--allow-root"]
