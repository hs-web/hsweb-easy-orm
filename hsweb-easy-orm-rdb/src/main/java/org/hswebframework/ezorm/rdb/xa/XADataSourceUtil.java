package org.hswebframework.ezorm.rdb.xa;

import com.atomikos.jdbc.AtomikosDataSourceBean;
import com.kingbase8.xa.KBXADataSource;

import javax.sql.XADataSource;
import java.util.Objects;

/**
 * Creates official Kingbase JDBC XA resources and wraps them for JTA enlistment.
 * The application owns the returned pool and must close it during shutdown.
 * Transaction decisions, logging and recovery belong to the JTA coordinator.
 */
public final class XADataSourceUtil {

    private XADataSourceUtil() {
    }

    public static KBXADataSource createKingbase(String jdbcUrl, String username, String password) {
        if (jdbcUrl == null || !jdbcUrl.startsWith("jdbc:kingbase8:")) {
            throw new IllegalArgumentException("A jdbc:kingbase8: URL is required");
        }
        KBXADataSource source = new KBXADataSource();
        source.setUrl(jdbcUrl);
        source.setUser(Objects.requireNonNull(username, "username"));
        source.setPassword(Objects.requireNonNull(password, "password"));
        return source;
    }

    public static AtomikosDataSourceBean managed(String resourceName, XADataSource source, int poolSize) {
        if (resourceName == null || resourceName.isBlank()) {
            throw new IllegalArgumentException("A stable XA resource name is required");
        }
        if (poolSize < 1) {
            throw new IllegalArgumentException("poolSize must be positive");
        }
        AtomikosDataSourceBean pool = new AtomikosDataSourceBean();
        pool.setUniqueResourceName(resourceName);
        pool.setXaDataSource(Objects.requireNonNull(source, "source"));
        pool.setMinPoolSize(1);
        pool.setMaxPoolSize(poolSize);
        pool.setBorrowConnectionTimeout(5);
        pool.setTestQuery("SELECT 1");
        return pool;
    }
}
