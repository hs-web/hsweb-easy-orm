package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.mapList;

public class LikeIgnoreCaseH2IntegrationTest {

    private static final String TABLE = "like_ignore_case_test";

    private DefaultDatabaseOperator operator;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        database.setCurrentSchema(schema);
        database.addSchema(schema);
        database.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));

        operator = DefaultDatabaseOperator.of(database);
        operator.ddl()
                .createOrAlter(TABLE)
                .addColumn().name("id").varchar(32).primaryKey().commit()
                .addColumn().name("name").varchar(64).commit()
                .commit()
                .sync();

        operator.dml()
                .insert(TABLE)
                .value("id", "1")
                .value("name", "Alpha")
                .execute()
                .sync();
    }

    @After
    public void cleanup() {
        operator.sql().sync().execute(
                org.hswebframework.ezorm.rdb.executor.SqlRequests.of("drop table " + TABLE)
        );
    }

    @Test
    public void shouldMatchDifferentCaseOnlyWhenIgnoreCaseIsEnabled() {
        List<Map<String, Object>> normal = operator.dml()
                .query(TABLE)
                .where(Term.of("name", "like", "alpha"))
                .fetch(mapList())
                .sync();
        Assert.assertEquals(0, normal.size());

        List<Map<String, Object>> ignoreCase = operator.dml()
                .query(TABLE)
                .where(Term.of("name", "like$ignoreCase", "alpha"))
                .fetch(mapList())
                .sync();
        Assert.assertEquals(1, ignoreCase.size());
    }
}
