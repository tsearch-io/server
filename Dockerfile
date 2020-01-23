FROM ubuntu:16.04
RUN mkdir -p /opt/my-app/
ARG BINARY_PATH                 # The path to our haskell binary, we provide it via ENV variable later
WORKDIR /opt/my-app
RUN apt-get update && apt-get install -y \
  ca-certificates \             # Here you could add more dependencies needed, e.g. I depend on tesseract-ocr
  libgmp-dev
ENV LANG C.UTF-8                # If you are parsing text from somewhere, this sets a locale for you and prevents weird behavior
ENV LC_ALL C.UTF-8
COPY "$BINARY_PATH" /opt/my-app # Coppying the Haskell binary
CMD ["/opt/my-app/my-app-exe"]  # my-app-exe needs to be the real name of the Haskell excecutable you are copying in
