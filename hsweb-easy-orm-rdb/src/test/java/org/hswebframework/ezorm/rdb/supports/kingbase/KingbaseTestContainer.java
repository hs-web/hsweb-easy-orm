package org.hswebframework.ezorm.rdb.supports.kingbase;

import org.junit.rules.ExternalResource;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.utility.DockerImageName;

import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Opt in with -Dkingbase.testcontainers=true; otherwise keep the external database configuration.
 * Each test class owns its container and restores connection properties after cleanup.
 */
public class KingbaseTestContainer extends ExternalResource {

    private static final String IMAGE =
        "yanhao98/kingbase_v009r001c010b0004_single_x86"
            + "@sha256:f5767b14d0309a7d608249ff4915d4eae35518c7388eaa758a63f8d14355cfcf";

    private static final String USERNAME = "system";
    private static final String PASSWORD = "123456";

    private final String mode;
    private final Map<String, String> originalProperties = new LinkedHashMap<>();
    private GenericContainer<?> container;

    public KingbaseTestContainer(String mode) {
        if (!"mysql".equals(mode) && !"oracle".equals(mode)) {
            throw new IllegalArgumentException("Unsupported Kingbase compatibility mode: " + mode);
        }
        this.mode = mode;
    }

    @Override
    protected void before() {
        if (!Boolean.getBoolean("kingbase.testcontainers")) {
            return;
        }
        container = new GenericContainer<>(DockerImageName.parse(IMAGE))
            .withPrivilegedMode(true)
            .withExposedPorts(54321)
            .withEnv("DB_MODE", mode)
            .withEnv("DB_USER", USERNAME)
            .withEnv("DB_PASSWORD", PASSWORD)
            .withEnv("NEED_START", "yes")
            // The image uses the invalid initdb option --enable_ci for Oracle when enabled.
            .withEnv("ENABLE_CI", "oracle".equals(mode) ? "no" : "yes")
            .withEnv("ENCODING", "UTF-8")
            .withEnv("TZ", "Asia/Shanghai")
            .withCommand("/usr/sbin/init")
            .withLogConsumer(new Slf4jLogConsumer(LoggerFactory.getLogger(KingbaseTestContainer.class))
                .withPrefix("kingbase-" + mode))
            .waitingFor(Wait.forSuccessfulCommand(
                "KINGBASE_PASSWORD=" + PASSWORD
                    + " /home/kingbase/install/kingbase/bin/ksql"
                    + " -h 127.0.0.1 -p 54321 -U " + USERNAME
                    + " -d test -w -t -A -c 'show database_mode' | grep -Fx '" + mode + "'"))
            .withStartupTimeout(Duration.ofMinutes(2));

        try {
            container.start();
            String url = "r2dbc:kingbase://" + container.getHost() + ":"
                + container.getMappedPort(54321) + "/test?mode=" + mode;
            configure("url", url);
            configure("username", USERNAME);
            configure("password", PASSWORD);
            configure("schema", "public");
            configure("integration", "true");
            System.out.println("[Kingbase Testcontainers] 模式=" + mode
                + "，地址=" + url + "，容器=" + container.getContainerId());
        } catch (RuntimeException | Error error) {
            after();
            throw error;
        }
    }

    private void configure(String name, String value) {
        String key = "kingbase." + mode + ".r2dbc." + name;
        originalProperties.put(key, System.getProperty(key));
        System.setProperty(key, value);
    }

    @Override
    protected void after() {
        try {
            if (container != null) {
                container.stop();
            }
        } finally {
            container = null;
            originalProperties.forEach((key, value) -> {
                if (value == null) {
                    System.clearProperty(key);
                } else {
                    System.setProperty(key, value);
                }
            });
            originalProperties.clear();
        }
    }
}
