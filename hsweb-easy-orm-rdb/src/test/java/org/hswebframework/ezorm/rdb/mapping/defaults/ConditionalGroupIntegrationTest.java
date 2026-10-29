package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.rdb.TestJdbcReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.sql.SQLException;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

/** 使用正式同步/响应式构造器执行 SQL，验证已有 OR 不能逃离后续交集。 */
@RunWith(Parameterized.class)
public class ConditionalGroupIntegrationTest {
    @Parameterized.Parameters(name = "reactive={0}, operation={1}")
    public static Object[][] cases() {
        return new Object[][]{{false, "query"}, {true, "query"}, {false, "update"},
            {true, "update"}, {false, "delete"}, {true, "delete"}};
    }

    @Parameterized.Parameter(0) public boolean reactive;
    @Parameterized.Parameter(1) public String operation;
    private H2ConnectionProvider connection;
    private DefaultSyncRepository<TestEntity, String> sync;
    private DefaultReactiveRepository<TestEntity, String> async;

    @Before
    public void init() {
        connection = new H2ConnectionProvider();
        var metadata = new RDBDatabaseMetadata(Dialect.H2);
        var schema = new H2SchemaMetadata("PUBLIC");
        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(new TestSyncSqlExecutor(connection));
        metadata.addFeature(new TestJdbcReactiveSqlExecutor(connection));
        var operator = DefaultDatabaseOperator.of(metadata);
        operator.ddl().createOrAlter("entity_test")
            .addColumn("id").primaryKey().varchar(32).commit()
            .addColumn("name").varchar(32).commit()
            .addColumn("state").number(4).commit()
            .addColumn("create_time").alias("createTime").datetime().commit()
            .commit().sync();
        var parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(metadata);
        var table = parser.parseTableMetadata(TestEntity.class).orElseThrow(AssertionError::new);
        schema.addTable(table);
        var wrapper = new EntityResultWrapper<>(TestEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeature(
            MappingFeatureType.columnPropertyMapping.createFeatureId(TestEntity.class)).orElseThrow(AssertionError::new));
        sync = new DefaultSyncRepository<>(operator, table, TestEntity.class, wrapper);
        async = new DefaultReactiveRepository<>(operator, table, TestEntity.class, wrapper);
        for (String id : List.of("outside", "allowed", "same-name", "unselected")) {
            var entity = new TestEntity();
            entity.setId(id);
            entity.setName(id.equals("allowed") || id.equals("same-name") ? "shared" : id);
            entity.setState((byte) (id.equals("allowed") || id.equals("unselected") ? 1 : 0));
            sync.insert(entity);
        }
    }

    @After
    public void close() throws SQLException {
        if (connection != null) connection.getConnection().close();
    }

    @Test
    public void existingOrAndLaterConstraintIntersectInSql() {
        if (operation.equals("query")) {
            List<TestEntity> result;
            if (reactive) {
                var query = async.createQuery().select("id", "name");
                result = restricted(query).fetch().collectList().block();
            } else {
                var query = sync.createQuery().select("id", "name");
                // 分组也必须支持调用方提供的不可变条件列表。
                query.is("id", "outside").or("name", "eq", "shared");
                query.getParam().setTerms(List.copyOf(query.getParam().getTerms()));
                result = query.group().group().nest().is("state", 1).end().fetch();
            }
            assertEquals(Set.of("allowed"), result.stream().map(TestEntity::getId).collect(Collectors.toSet()));
            assertNull(result.get(0).getState());
        } else if (operation.equals("update")) {
            int changed = reactive
                ? restricted(async.createUpdate().set("name", "changed").set("state", 7)
                    .includes("name", "state").excludes("state")).execute().block()
                : restricted(sync.createUpdate().set("name", "changed").set("state", 7)
                    .includes("name", "state").excludes("state")).execute();
            assertEquals(1, changed);
            assertEquals("changed", sync.findById("allowed").orElseThrow().getName());
            assertEquals(Byte.valueOf((byte) 1), sync.findById("allowed").orElseThrow().getState());
        } else {
            int changed = reactive ? restricted(async.createDelete()).execute().block()
                : restricted(sync.createDelete()).execute();
            assertEquals(1, changed);
            assertFalse(sync.findById("allowed").isPresent());
        }
        assertEquals("outside", sync.findById("outside").orElseThrow().getName());
        assertEquals("shared", sync.findById("same-name").orElseThrow().getName());
        assertEquals("unselected", sync.findById("unselected").orElseThrow().getName());
    }

    private static <T extends Conditional<T>> T restricted(T conditional) {
        conditional.is("id", "outside").or("name", "eq", "shared");
        assertSame(conditional, conditional.group().group().nest().is("state", 1).end());
        return conditional;
    }
}
