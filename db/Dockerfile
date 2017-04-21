FROM postgres:latest
ENV POSTGRES_USER climbing-scheduler
ENV POSTGRES_PASSWORD climbing-scheduler
ENV POSTGRES_DB climbing-scheduler
ADD CreateDb.sql /docker-entrypoint-initdb.d/
