package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.core.param.UpdateParam;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;
import java.util.List;

public class DefaultQueryUpdateCoverageTest {

    private DefaultSyncRepository<TestEntity, String> repository;

    @Before
    public void init() {
        RDBDatabaseMetadata databaseMetadata = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata h2 = new H2SchemaMetadata("PUBLIC");
        databaseMetadata.setCurrentSchema(h2);
        databaseMetadata.addSchema(h2);
        databaseMetadata.addFeature(new TestSyncSqlExecutor(new H2ConnectionProvider()));

        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(databaseMetadata);
        operator.ddl()
            .createOrAlter("entity_test")
            .addColumn("id").primaryKey().varchar(32).commit()
            .addColumn("name").varchar(32).commit()
            .addColumn("state").number(4).commit()
            .addColumn("create_time").alias("createTime").datetime().commit()
            .commit()
            .sync();

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(databaseMetadata);
        RDBTableMetadata table = parser.parseTableMetadata(TestEntity.class).orElseThrow(NullPointerException::new);
        h2.addTable(table);

        EntityResultWrapper<TestEntity> wrapper = new EntityResultWrapper<>(TestEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(TestEntity.class))
                                .orElseThrow(NullPointerException::new));

        repository = new DefaultSyncRepository<>(DefaultDatabaseOperator.of(databaseMetadata), table, TestEntity.class, wrapper);
    }

    @Test
    public void testQueryBranches() {
        repository.save(entity("q1", "alpha", (byte) 1));
        repository.save(entity("q2", "beta", (byte) 2));

        QueryParam param = repository.createQuery()
            .select("id", "name", "createTime")
            .selectExcludes("createTime")
            .context("traceId", "trace-1")
            .orderBy(SortOrder.desc("name"))
            .paging(0, 1)
            .getParam();
        Assert.assertEquals("trace-1", param.getContext("traceId").orElse(null));
        Assert.assertEquals(1, param.getPageSize());
        Assert.assertTrue(param.isPaging());
        Assert.assertEquals(0, param.getSorts().size());
        Assert.assertFalse(param.isForUpdate());

        List<TestEntity> page = repository.createQuery()
            .select("id", "name", "createTime")
            .selectExcludes("createTime")
            .context("traceId", "trace-1")
            .orderBy(SortOrder.desc("name"))
            .paging(0, 1)
            .fetch();
        Assert.assertEquals(1, page.size());
        Assert.assertNull(page.get(0).getCreateTime());

        Assert.assertEquals("alpha", repository.createQuery()
            .select("id", "name")
            .where("id", "q1")
            .fetchOne()
            .orElseThrow(NullPointerException::new)
            .getName());

        Assert.assertEquals(2, repository.createQuery()
            .where()
            .and("state", TermType.gte, 1)
            .count());

        Assert.assertEquals(1, repository.createQuery()
            .where("id", "q1")
            .forUpdate()
            .fetch()
            .size());

        QueryParam manual = new QueryParam();
        manual.setPaging(false);
        manual.getTerms().add(Term.of("id", TermType.eq, "q1"));
        Assert.assertSame(manual, repository.createQuery().setParam(manual).getParam());
    }

    @Test
    public void testUpdateBranches() {
        TestEntity source = entity("u1", "before", (byte) 1);
        repository.save(source);

        Assert.assertEquals(1, repository.createUpdate()
            .set(source)
            .includes("name")
            .where("id", "u1")
            .execute());
        Assert.assertEquals("before", repository.findById("u1").orElseThrow(NullPointerException::new).getName());

        Assert.assertEquals(1, repository.createUpdate()
            .set("name", "after")
            .setNull("createTime")
            .where("id", "u1")
            .execute());
        TestEntity updated = repository.findById("u1").orElseThrow(NullPointerException::new);
        Assert.assertEquals("after", updated.getName());
        Assert.assertNull(updated.getCreateTime());

        UpdateParam<TestEntity> param = new UpdateParam<>(entity("u1", "param", (byte) 3));
        param.getIncludes().add("name");
        param.getExcludes().add("state");
        param.getTerms().add(Term.of("id", TermType.eq, "u1"));
        Assert.assertEquals(1, repository.createUpdate().accept(param).execute());

        updated = repository.findById("u1").orElseThrow(NullPointerException::new);
        Assert.assertEquals("param", updated.getName());
        Assert.assertEquals(Byte.valueOf((byte) 1), updated.getState());

        Assert.assertEquals(0, repository.updateById(null, source));
        Assert.assertEquals(0, repository.updateById("u1", null));
        Assert.assertEquals(0, repository.deleteById((java.util.Collection<String>) null));
        Assert.assertEquals(0, repository.deleteById(java.util.Collections.emptyList()));
        Assert.assertTrue(repository.findById((String) null).isEmpty());
        Assert.assertTrue(repository.findById(java.util.Collections.emptyList()).isEmpty());
    }

    private TestEntity entity(String id, String name, Byte state) {
        TestEntity entity = new TestEntity();
        entity.setId(id);
        entity.setName(name);
        entity.setState(state);
        entity.setCreateTime(new Date());
        return entity;
    }
}
