package org.hswebframework.ezorm.rdb.supports;

import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.RecordResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.wrapper.NestedEntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.Terms;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

@Slf4j
public abstract class BasicCommonTests {

    protected SyncRepository<BasicTestEntity, String> repository;
    protected SyncRepository<Record, String> addressRepository;

    protected abstract RDBSchemaMetadata getSchema();

    protected abstract Dialect getDialect();

    protected abstract SyncSqlExecutor getSqlExecutor();

    protected RDBDatabaseMetadata getDatabase() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(getDialect());

        RDBSchemaMetadata schema = getSchema();
        schema.addFeature(new DefaultValueGenerator() {
            @Override
            public String getSortId() {
                return "uuid";
            }

            @Override
            public RuntimeDefaultValue generate(ObjectMetadata meta) {
                return () -> UUID.randomUUID().toString().replace("-", "");
            }

            @Override
            public String getName() {
                return "UUID";
            }
        });
        log.debug(schema.toString());

        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(getSqlExecutor());

        return metadata;
    }


    @Before
    public void init() {
        RDBDatabaseMetadata metadata = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(metadata);

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(metadata);

        parser.parseTableMetadata(Address.class)
              .ifPresent(address -> {
                  operator.ddl()
                          .createOrAlter(address)
                          .commit()
                          .sync();
              });

        RDBTableMetadata table = parser
                .parseTableMetadata(BasicTestEntity.class)
                .orElseThrow(NullPointerException::new);

        //  table.addFeature((EventListener) (type, context) -> log.debug("event:{},context:{}", type, context));

        operator.ddl()
                .createOrAlter(table)
                .commit()
                .sync();
        NestedEntityResultWrapper wrapper =
                table
                        .<EntityColumnMapping>getFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(BasicTestEntity.class))
                        .map(mapping->new NestedEntityResultWrapper(mapping))
                        .orElseThrow(NullPointerException::new);

        repository = new DefaultSyncRepository<>(operator, table, BasicTestEntity.class, wrapper);
        addressRepository = operator.dml().createRepository("test_address");
    }

    @After
    public void after() {
        try {
            getSqlExecutor().execute(SqlRequests.of("drop table entity_test_table"));

        } catch (Exception e) {

        }
        try {
            getSqlExecutor().execute(SqlRequests.of("drop table test_address"));

        } catch (Exception e) {

        }
    }

    @Test
    public void testRepositoryInsertBach() {
        List<BasicTestEntity> entities = Flux
                .range(0, 100)
                .map(integer -> BasicTestEntity.builder()
                                               // .id("test_id_" + integer)
                                               .balance(1000L)
                                               .name("test:" + integer)
                                               .createTime(new Date())
                                               .tags(Arrays.asList("a", "b", "c", "d"))
                                               .state((byte) 1)
                                               // .stateEnum(StateEnum.enabled)
                                               .build())
                .collectList().block();
        Assert.assertEquals(100, repository.insertBatch(entities));
    }

    @Test
    public void testInsertDuplicate() {
        //10次insert
        Assert.assertEquals(3, repository.insertBatch(
                Stream
                        .of(1, 2, 2, 3, 1, 3)
                        .map(integer -> BasicTestEntity
                                .builder()
                                .id("test_dup_" + integer)
                                .balance(1000L)
                                .name("test2:" + integer)
                                .createTime(new Date())
                                .tags(Arrays.asList("a", "b", "c", "d"))
                                .state((byte) 1)
                                .stateEnum(StateEnum.enabled)
                                .build())
                        .collect(Collectors.toList())
        ));
        ;

        Assert.assertEquals(3,
                            repository.createDelete()
                                      .like$(BasicTestEntity::getId, "test_dup_")
                                      .execute());
    }

    @Test
    public void testRepositorySave() {
        BasicTestEntity entity = BasicTestEntity
                .builder()
                .id("test_id_save")
                .balance(1000L)
                .name("test")
                .createTime(new Date())
                .tags(Arrays.asList("a", "b", "c", "d"))
                .state((byte) 1)
//                .aTest("test")
                .addressId("test")
                .doubleVal(1D)
                .bigDecimal(new BigDecimal("1.2"))
                .stateEnum(StateEnum.enabled)
                .build();

        Assert.assertEquals(repository.save(entity).getTotal(), 1);
        {
            BasicTestEntity inBase = repository
                    .createQuery()
                    .select("*")
                    .where("id", entity.getId())
                    .fetchOne()
                    .orElseThrow(UnsupportedOperationException::new);

            Assert.assertEquals(entity, inBase);

        }

        entity.setStateEnum(null);
        Assert.assertEquals(repository.save(entity).getTotal(), 1);

        BasicTestEntity inBase = repository
                .createQuery()
                .select("*")
                .where("id", entity.getId())
                .fetchOne()
                .orElseThrow(UnsupportedOperationException::new);

        Assert.assertEquals(StateEnum.enabled, inBase.getStateEnum());


    }

    @Test
    public void testEnums() {
        BasicTestEntity entity = BasicTestEntity
                .builder()
                .id("enums_id")
                .balance(1000L)
                .name("test")
                .createTime(new Date())
                .tags(Arrays.asList("a", "b", "c", "d"))
                .state((byte) 1)
                .addressId("test")
                .stateEnum(StateEnum.enabled)
                .stateEnums(new StateEnum[]{StateEnum.enabled, StateEnum.disabled})
                .build();
        repository.insert(entity);


        Assert.assertFalse(repository
                                   .createQuery()
                                   .select("id", "stateEnums")
                                   .in(BasicTestEntity::getStateEnums, StateEnum.enabled)
                                   .fetchOne()
                                   .isPresent()
        );

        Assert.assertTrue(repository
                                  .createQuery()
                                  .select("id", "stateEnums")
                                  .in(BasicTestEntity::getStateEnums, StateEnum.enabled, StateEnum.disabled)
                                  .fetchOne()
                                  .isPresent());

        Assert.assertTrue(repository
                                  .createQuery()
                                  .select("id", "stateEnums")
                                  .where(Terms.enumInAny(BasicTestEntity::getStateEnums, StateEnum.disabled))
                                  .fetchOne()
                                  .isPresent());


        Assert.assertTrue(repository
                                  .createQuery()
                                  .select("id", "stateEnums")
                                  .where(Terms.enumNotInAny(BasicTestEntity::getStateEnums, StateEnum.warn))
                                  .fetchOne()
                                  .isPresent());

    }


    @Test
    public void testJoin() {

        BasicTestEntity entity = BasicTestEntity
                .builder()
                .id("joinTest")
                .balance(1000L)
                .name("test")
                .createTime(new Date())
                .tags(Arrays.asList("a", "b", "c", "d"))
                .state((byte) 1)
                .addressId("joinTest")
                .stateEnum(StateEnum.enabled)
                .build();
        addressRepository.insert(Record.newRecord().putValue("id", "joinTest").putValue("name", "joinTest"));
        repository.insert(entity);

        BasicTestEntity e = repository.findById(entity.getId()).orElse(null);
        Assert.assertNotNull(e);
        Assert.assertNotNull(e.getAddress());
        Assert.assertEquals(e.getAddress().getName(),"joinTest");

    }

    @Test
    public void testRepositoryCurd() {


        BasicTestEntity entity = BasicTestEntity
                .builder()
                .id("test_id")
                .balance(1000L)
                .name("test")
                .createTime(new Date())
                .tags(Arrays.asList("a", "b", "c", "d"))
                .state((byte) 1)
                .addressId("test")
                .stateEnum(StateEnum.enabled)
                .build();
        addressRepository.insert(Record.newRecord().putValue("id", "test").putValue("name", "test_address"));
        repository.insert(entity);

        Assert.assertEquals(repository.findById("test_id").orElseThrow(NullPointerException::new), entity);

        List<BasicTestEntity> list = repository.createQuery()
                                               .selectExcludes("address.*")
                                               .where(entity::getId)
                                               .nest()
                                               .is(entity::getId).or().is("address.name", "test")
                                               .end()
                                               .orderBy(SortOrder.desc("id"))
                                               .paging(0, 10)
                                               .fetch();

        Assert.assertEquals(list.get(0), entity);

        Assert.assertEquals(1, repository.createUpdate()
                                         .set(entity::getState)
                                         .nest()
                                         .is(entity::getId).or().is(entity::getId)
                                         .end()
                                         .where(entity::getId).and().is("address.name", "test_address")
                                         .execute());

        Assert.assertEquals(1, repository.createDelete()
                                         .where(entity::getId)
                                         .nest()
                                         .is(entity::getId).or().is(entity::getId)
                                         .end().is("address.name", "test_address")
                                         .execute());

    }

    @Test
    public void testPager() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_pager")
                    .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
                    .commit()
                    .sync();

            InsertOperator insert = operator.dml()
                                            .insert("test_pager")
                                            .columns("id");

            for (int i = 0; i < 100; i++) {
                insert.values(i + 1);
            }
            insert.execute().sync();

            for (int i = 0; i < 10; i++) {
                long sum = operator.dml()
                                   .query("test_pager")
                                   .select("id")
//                        .orderBy(desc("id"))
                                   .paging(i, 10)
                                   .fetch(mapStream())
                                   .sync()
                                   .map(map -> map.get("id"))
                                   .map(Number.class::cast)
                                   .mapToInt(Number::intValue)
                                   .sum();

                Assert.assertEquals(sum, (((i * 10) + 1) + ((i + 1) * 10)) * 10 / 2);
            }
        } finally {
            try {
                operator
                        .sql()
                        .sync()
                        .execute(SqlRequests.of("drop table " + database.getCurrentSchema().getName() + ".test_pager"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Test
    public void testCrud() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        try {
            operator.ddl()
                    .createOrAlter("test_dml_crud")
                    .alias("dml")
                    .comment("dmlTest")
                    .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                    .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                    .addColumn().name("create_time").datetime().comment("创建日期").commit()
                    .addColumn().name("state").number(4).comment("状态").commit()
                    .addColumn().name("comment").varchar(32).defaultValueNative("'1'").commit()
                    .commit().sync();

            operator.dml()
                    .insert("test_dml_crud")
                    .value("id", "1234")
                    .value("name", "名称")
                    .value("state", 1)
                    .value("create_time", new Date())
                    .execute()
                    .sync();

            int updated = operator.dml()
                                  .update("test_dml_crud")
                                  .set("comment", "2")
                                  .where(dsl -> dsl.where("id", "1234"))
                                  .execute()
                                  .sync();

            Assert.assertEquals(updated, 1);

            int sum = operator.dml()
                              .query("test_dml_crud")
                              .fetch(lowerCase(mapStream()))
                              .sync()
                              .map(map -> map.get("comment"))
                              .map(String::valueOf)
                              .mapToInt(Integer::valueOf)
                              .sum();
            Assert.assertEquals(sum, 2);

            int deleted = operator.dml().delete("test_dml_crud")
                                  .where(dsl -> dsl.where("comment", "2"))
                                  .execute()
                                  .sync();
            Assert.assertEquals(deleted, 1);

        } finally {
            try {
                operator
                        .sql()
                        .sync()
                        .execute(SqlRequests.of("drop table " + database
                                .getCurrentSchema()
                                .getName() + ".test_dml_crud"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Test
    public void testDDL() {

        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .alias("test")
                    .comment("测试")
                    .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                    .addColumn().name("name").varchar(64).notNull().comment("名称").commit()
                    .addColumn().name("comment").varchar(32).defaultValueNative("'1'").commit()

                    .addColumn().name("number_test").number(4).commit()
                    .addColumn().name("date_test").datetime().commit()
                    .addColumn().name("int_test").integer().commit()
                    .addColumn().name("bigint_test").bigint().commit()
                    .addColumn().name("clob_test").clob().commit()
                    .addColumn().name("tinyint_test").tinyint().commit()
                    .addColumn().name("blob_test").type(JDBCType.BLOB, byte[].class).commit()
                    .addColumn().name("char_test").type(JDBCType.CHAR, String.class).length(32).commit()
                    .addColumn().name("time_test").type(JDBCType.TIME, LocalTime.class).commit()
                    .addColumn().name("date_test").type(JDBCType.DATE, LocalDate.class).commit()

                    .index().name("index_").column("name").commit()
                    .commit().sync();

            //alter
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .addColumn().name("name").varchar(128).comment("名称").commit()
                    .addColumn().name("test").varchar(32).comment("test").commit()
                    .addColumn().name("age").number(4).defaultValueNative("0").comment("年龄").commit()
                    .commit().sync();

            //drop column
            operator.ddl()
                    .createOrAlter("test_ddl_create")
                    .dropColumn("test")
                    .commit().sync();


        } finally {
            try {
                operator
                        .sql()
                        .sync()
                        .execute(SqlRequests.of("drop table " + database
                                .getCurrentSchema()
                                .getName() + ".test_ddl_create"));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

}
