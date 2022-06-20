package org.hswebframework.ezorm.rdb;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.utility.DockerImageName;

public class Containers {

    public static GenericContainer<?> newMysql(String version) {
        return new GenericContainer<>(DockerImageName.parse("mysql:" + System.getProperty("test.mysql.version", version)))
                .withEnv("TZ", "Asia/Shanghai")
                .withEnv("MYSQL_ROOT_PASSWORD", "root")
                .withEnv("MYSQL_DATABASE", "ezorm")
                .withCommand("--character-set-server=utf8mb4")
                .withExposedPorts(3306)
                .waitingFor(Wait.forListeningPort());
//                .waitingFor(Wait.forLogMessage(".*ready for connections.*",1));
    }

    public static GenericContainer<?> newPostgresql(String version) {
        return new GenericContainer<>(DockerImageName.parse("postgres:" + System.getProperty("test.postgres.version", version)) + "-alpine")
                .withEnv("TZ", "Asia/Shanghai")
                .withEnv("POSTGRES_PASSWORD", "admin")
                .withEnv("POSTGRES_DB", "ezorm")
                .withCommand("postgres", "-c", "max_connections=500")
                .withExposedPorts(5432)
                .waitingFor(Wait.forListeningPort());
//                .waitingFor(Wait.forLogMessage(".*database system is ready to accept connections.*",1));
    }

    public static GenericContainer<?> newOracle() {
        return new GenericContainer<>(DockerImageName.parse("iatebes/oracle_11g"))
                .withEnv("TZ", "Asia/Shanghai")
                .withExposedPorts(1521)
                .waitingFor(Wait.forLogMessage(".*opened.*", 1));
    }

    public static GenericContainer<?> newMSSQL() {
        return new GenericContainer<>(DockerImageName.parse("mcr.microsoft.com/mssql/server:2017-latest-ubuntu"))
                .withEnv("TZ", "Asia/Shanghai")
                .withEnv("SA_PASSWORD","ezorm@PasswOrd")
                .withEnv("ACCEPT_EULA","y")
                .withEnv("MSSQL_PID","Enterprise")
                .withExposedPorts(1433)
                .waitingFor(Wait.forLogMessage(".*Service Broker manager has started.*",1));
    }

}
