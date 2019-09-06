package org.hswebframework.ezorm.rdb.supports;

import org.hswebframework.ezorm.ConnectionProvider;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;

public abstract class CommonDatabaseOperatorTest {

    protected abstract RDBDatabaseMetadata getDatabase();

    protected abstract ConnectionProvider getConnectionProvider();


}
