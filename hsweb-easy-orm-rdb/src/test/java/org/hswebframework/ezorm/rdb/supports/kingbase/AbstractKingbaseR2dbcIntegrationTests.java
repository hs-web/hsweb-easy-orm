package org.hswebframework.ezorm.rdb.supports.kingbase;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.codec.ClobValue;
import org.hswebframework.ezorm.rdb.codec.ClobValueCodec;
import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.SaveResult;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.NestedEntityResultWrapper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.parser.TableMetadataParser;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.Terms;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.operator.dml.query.Selects;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.hswebframework.ezorm.rdb.supports.Address;
import org.hswebframework.ezorm.rdb.supports.BasicTestEntity;
import org.hswebframework.ezorm.rdb.supports.StateEnum;
import org.junit.After;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.hswebframework.ezorm.core.param.Term;
import reactor.test.StepVerifier;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static io.r2dbc.spi.ConnectionFactoryOptions.PASSWORD;
import static io.r2dbc.spi.ConnectionFactoryOptions.USER;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.lowerCase;

/**
 * Kingbase MySQL 和 Oracle 兼容模式共用的真实数据库测试。
 * setUp 中配置本地连接默认值，也可通过系统属性或环境变量覆盖。
 */
public abstract class AbstractKingbaseR2dbcIntegrationTests {

    private static final Duration TIMEOUT = Duration.ofSeconds(30);

    private ConnectionFactory connectionFactory;

    private ReactiveSqlExecutor executor;

    private RDBSchemaMetadata schema;

    private DatabaseOperator operator;

    private RDBTableMetadata table;

    private String tableName;

    private String indexName;

    protected abstract String mode();

    protected abstract String defaultUrl();

    protected abstract RDBSchemaMetadata createSchema(String schemaName);

    @Before
    public void setUp() {
        Assume.assumeTrue(
            "Set -D" + prefix() + ".integration=true to run the Kingbase integration tests",
            Boolean.getBoolean(prefix() + ".integration")
                || Boolean.getBoolean(prefix() + ".enabled")
        );
        String url = configuration("url", defaultUrl());
        String username = configuration("username", "system");
        String password = configuration("password", "1234");
        connectionFactory = ConnectionFactories.get(
            ConnectionFactoryOptions.parse(url)
                .mutate()
                .option(USER, username)
                .option(PASSWORD, password)
                .build()
        );
        Assert.assertEquals("KingbaseES", connectionFactory.getMetadata().getName());
        // 实际建立连接并查询兼容模式，确认连接可用后再初始化测试表。
        Connection connection = Mono.from(connectionFactory.create()).block(TIMEOUT);
        Assert.assertNotNull("Kingbase connection was not created", connection);
        try {
            Assert.assertEquals(mode(), queryDatabaseMode(connection));
        } finally {
            Mono.from(connection.close()).block(TIMEOUT);
        }
        System.out.println("[数据库连接] 已连接 " + url + "，用户=" + username + "，兼容模式=" + mode());

        R2dbcConnectionProvider connectionProvider = new R2dbcConnectionProvider() {
            @Override
            public Mono<Connection> getConnection() {
                return Mono.defer(() -> Mono.from(connectionFactory.create()));
            }

            @Override
            public void releaseConnection(Connection connection) {
                // The executor below performs the asynchronous close itself.
            }
        };
        executor = new TestReactiveSqlExecutor(connectionProvider) {
            @Override
            protected <T> Flux<T> doInConnection(Function<Connection, Publisher<T>> handler) {
                return Flux.usingWhen(
                    connectionProvider.getConnection(),
                    handler,
                    connection -> Mono.from(connection.close()),
                    (connection, error) -> Mono.from(connection.close()),
                    connection -> Mono.from(connection.close())
                );
            }
        };

        schema = createSchema(configuration("schema", "public"));
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(executor);
        operator = DefaultDatabaseOperator.of(database);

        String suffix = UUID.randomUUID().toString().replace("-", "").substring(0, 12);
        tableName = "ez_kb_" + mode().substring(0, 2) + "_" + suffix;

        indexName = "idx_" + mode().substring(0, 2) + "_" + suffix;
        operator.ddl()
                .createOrAlter(tableName)
                .comment("Kingbase R2DBC integration table")
                .addColumn("id").varchar(32).primaryKey().comment("Primary key").commit()
                .addColumn("name").varchar(64).notNull().comment("Display name").commit()
                .addColumn("position_no").integer().notNull().commit()
                .addColumn("nullable_text").varchar(64).commit()
                .addColumn("created_at").datetime().commit()
                .addColumn("content").clob().commit()
                .addColumn("json_content").clob().commit()
                .index().name(indexName).column("name").commit()
                .commit()
                .reactive()
                .block(TIMEOUT);

        table = schema.getTable(tableName, false)
                      .orElseThrow(() -> new AssertionError("EasyORM did not retain the created table metadata"));
    }

    @After
    public void tearDown() {
        if (executor == null || table == null) {
            return;
        }
        executor.execute(SqlRequests.of("drop table " + table.getFullName()))
                .onErrorResume(error -> Mono.empty())
                .block(TIMEOUT);
    }

    /**
     * 验证 DDL 新增字段后，能从 Kingbase 重新读取字段和索引元数据，并支持字段名大小写查询。
     */
    @Test
    public void shouldCreateDdlAndReadCaseInsensitiveMetadata() {
        System.out.println("[DDL 测试] 模式=" + mode() + "，表=" + tableName
            + "，验证新增字段、元数据回读及大小写查询");
        // 初始化阶段已建表，这里通过 createOrAlter 为已有表添加字段。
        operator.ddl()
                .createOrAlter(tableName)
                .addColumn("alter_note").varchar(32).commit()
                .commit()
                .reactive()
                .block(TIMEOUT);

        // 使用新的元数据实例读取数据库，避免仅验证内存中的表结构。
        RDBSchemaMetadata reloadedSchema = createSchema(schema.getName());
        RDBDatabaseMetadata reloadedDatabase = new RDBDatabaseMetadata(reloadedSchema.getDialect());
        reloadedDatabase.addSchema(reloadedSchema);
        reloadedDatabase.setCurrentSchema(reloadedSchema);
        reloadedDatabase.addFeature(executor);

        RDBTableMetadata reloaded = reloadedSchema.getTableReactive(tableName, true).block(TIMEOUT);

        Assert.assertNotNull(reloaded);
        System.out.println("[元数据回读] 新增字段 alter_note=" + reloaded.getColumn("alter_note").isPresent()
            + "，小写 id=" + reloaded.getColumn("id").isPresent()
            + "，大写 ID=" + reloaded.getColumn("ID").isPresent()
            + "，索引数=" + reloaded.getIndexes().size());
        Assert.assertTrue(reloaded.getColumn("id").isPresent());
        Assert.assertTrue(reloaded.getColumn("ID").isPresent());
        Assert.assertTrue(reloaded.getColumn("created_at").isPresent());
        Assert.assertTrue(reloaded.getColumn("alter_note").isPresent());
        Assert.assertFalse(reloaded.getIndexes().isEmpty());

        // 按名称读取索引，确认索引名称及所属表均正确。
        RDBIndexMetadata index = reloadedSchema
            .findFeatureNow(IndexMetadataParser.ID)
            .parseByNameReactive(indexName)
            .cast(RDBIndexMetadata.class)
            .block(TIMEOUT);
        Assert.assertNotNull(index);
        System.out.println("[索引回读] 名称=" + index.getName() + "，所属表=" + index.getTableName());
        Assert.assertEquals(indexName, index.getName());
        Assert.assertEquals(tableName, index.getTableName());

        // Oracle 兼容模式还需保留字符串字段的字符长度和类型信息。
        if ("oracle".equals(mode())) {
            RDBColumnMetadata nameColumn = reloaded.getColumn("name").orElseThrow(AssertionError::new);
            Assert.assertEquals(64, nameColumn.getLength());
            String dataType = nameColumn.getDataType().toLowerCase(Locale.ROOT);
            System.out.println("[Oracle 字符字段] name 类型=" + dataType + "，长度=" + nameColumn.getLength());
            Assert.assertTrue("Unexpected Oracle compatibility character type: " + dataType,
                dataType.contains("varchar") || dataType.contains("character varying"));
        }
    }

    /**
     * 对应原生表解析测试，按兼容模式建表后校验字段类型、长度、精度、小数位和非空属性。
     */
    @Test
    public void shouldReadColumnAttributesForCompatibilityMode() {
        boolean oracleMode = "oracle".equals(mode());
        String varcharType = oracleMode ? "varchar2" : "varchar";
        String ageType = oracleMode ? "number(10,0)" : "int";
        String amountType = oracleMode ? "number(12,2)" : "decimal(12,2)";
        RDBTableMetadata metadataTable = schema.newTable(tableName + "_columns");
        String createSql = "create table " + metadataTable.getFullName()
            + " (id " + varcharType + "(32) primary key, name " + varcharType
            + "(128) not null, age " + ageType + ", amount " + amountType + ")";
        System.out.println("[字段属性回读] 模式=" + mode() + "，建表 SQL=" + createSql);
        executor.execute(SqlRequests.of(createSql)).block(TIMEOUT);

        try {
            List<Map<String, Object>> databaseColumns = executor.select(
                SqlRequests.of("select column_name, data_type, "
                    + (oracleMode ? "" : "column_type, ") + "character_maximum_length, "
                    + "numeric_precision, numeric_scale, is_nullable "
                    + "from information_schema.columns where table_schema = ? and table_name = ? "
                    + "order by ordinal_position", schema.getName(), metadataTable.getName()),
                lowerCase(ResultWrappers.map())
            ).collectList().block(TIMEOUT);
            System.out.println("[数据库原始字段属性] 模式=" + mode() + "，" + databaseColumns);

            // 使用新的 Schema 从数据库加载，避免验证建表时的内存元数据。
            RDBSchemaMetadata reloadedSchema = createSchema(schema.getName());
            RDBDatabaseMetadata reloadedDatabase = new RDBDatabaseMetadata(reloadedSchema.getDialect());
            reloadedDatabase.addSchema(reloadedSchema);
            reloadedDatabase.setCurrentSchema(reloadedSchema);
            reloadedDatabase.addFeature(executor);
            RDBTableMetadata reloaded = reloadedSchema
                .getTableReactive(metadataTable.getName(), true)
                .block(TIMEOUT);
            Assert.assertNotNull(reloaded);

            RDBColumnMetadata id = reloaded.getColumnNow("id");
            RDBColumnMetadata name = reloaded.getColumnNow("name");
            RDBColumnMetadata age = reloaded.getColumnNow("age");
            RDBColumnMetadata amount = reloaded.getColumnNow("amount");
            for (RDBColumnMetadata column : new RDBColumnMetadata[]{id, name, age, amount}) {
                System.out.println("[字段实际属性] 模式=" + mode() + "，字段=" + column.getName()
                    + "，类型=" + column.getDataType() + "，JDBC=" + column.getSqlType()
                    + "，Java=" + column.getJavaType().getName() + "，长度=" + column.getLength()
                    + "，精度=" + column.getPrecision() + "，小数位=" + column.getScale()
                    + "，非空=" + column.isNotNull());
            }

            // MySQL 从 column_type 恢复字符长度，Oracle 从 character_maximum_length 读取。
            Assert.assertEquals("varchar(32)", id.getDataType());
            Assert.assertEquals(JDBCType.VARCHAR, id.getSqlType());
            Assert.assertEquals(String.class, id.getJavaType());
            Assert.assertEquals(32, id.getLength());
            Assert.assertTrue(id.isNotNull());

            Assert.assertEquals("varchar(128)", name.getDataType());
            Assert.assertEquals(JDBCType.VARCHAR, name.getSqlType());
            Assert.assertEquals(String.class, name.getJavaType());
            Assert.assertEquals(128, name.getLength());
            Assert.assertTrue(name.isNotNull());

            Assert.assertEquals(oracleMode ? JDBCType.NUMERIC : JDBCType.INTEGER, age.getSqlType());
            Assert.assertEquals(oracleMode ? BigDecimal.class : Integer.class, age.getJavaType());
            // Oracle 元数据返回 numeric，精度和小数位保存在独立属性中。
            Assert.assertEquals(oracleMode ? "numeric" : "integer", age.getDataType());
            // KES MySQL 兼容视图未提供 integer 的精度，当前 ORM 使用 0 表示未获取到精度。
            Assert.assertEquals(oracleMode ? 10 : 0, age.getPrecision());
            Assert.assertEquals(0, age.getScale());
            Assert.assertFalse(age.isNotNull());

            // 显式指定定点数精度和小数位，避免仅用 integer 的缺省值验证数值属性。
            Assert.assertEquals(JDBCType.NUMERIC, amount.getSqlType());
            Assert.assertEquals(BigDecimal.class, amount.getJavaType());
            Assert.assertEquals(oracleMode ? "numeric" : "decimal(12,2)", amount.getDataType());
            Assert.assertEquals(12, amount.getPrecision());
            Assert.assertEquals(2, amount.getScale());
            Assert.assertFalse(amount.isNotNull());
        } finally {
            executor.execute(SqlRequests.of("drop table " + metadataTable.getFullName())).block(TIMEOUT);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，id/name/age/amount 的字段类型、Java/JDBC 类型、长度、精度、小数位及非空属性符合预期");
    }

    /**
     * 对应原生 Oracle 的表枚举及 parseAll 场景，通过 R2DBC 批量反向解析后执行仓储操作。
     */
    @Test
    public void shouldEnumerateTablesAndReverseEngineerMetadata() {
        String metadataSchemaName = tableName + "_all";
        String quotedSchema = schema.getDialect().quote(metadataSchemaName, false);
        String firstTableName = "test_table_all";
        String secondTableName = "test_table_other";
        String firstTable = quotedSchema + "." + schema.getDialect().quote(firstTableName, false);
        String secondTable = quotedSchema + "." + schema.getDialect().quote(secondTableName, false);
        String quotedId = schema.getDialect().quote("id", false);
        boolean oracleMode = "oracle".equals(mode());
        String varcharType = oracleMode ? "varchar2" : "varchar";
        String ageType = oracleMode ? "number(10,0)" : "int";

        System.out.println("[表枚举与批量反向解析] 模式=" + mode() + "，临时 Schema=" + metadataSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            // 保留带引号的小写表名和 id 字段名，与原生 Oracle 反向解析测试对应。
            String createFirst = "create table " + firstTable + " (" + quotedId + " " + varcharType
                + "(32) primary key, name " + varcharType + "(128) not null, age " + ageType + ")";
            String createSecond = "create table " + secondTable + " (" + quotedId + " " + varcharType
                + "(32) primary key, name " + varcharType + "(64) not null)";
            System.out.println("[建表 SQL] " + createFirst);
            executor.execute(SqlRequests.of(createFirst)).block(TIMEOUT);
            System.out.println("[建表 SQL] " + createSecond);
            executor.execute(SqlRequests.of(createSecond)).block(TIMEOUT);

            RDBSchemaMetadata parsedSchema = createSchema(metadataSchemaName);
            RDBDatabaseMetadata parsedDatabase = new RDBDatabaseMetadata(parsedSchema.getDialect());
            parsedDatabase.addSchema(parsedSchema);
            parsedDatabase.setCurrentSchema(parsedSchema);
            parsedDatabase.addFeature(executor);
            TableMetadataParser parser = parsedSchema.findFeatureNow(TableMetadataParser.id);

            List<String> tableNames = parser.parseAllTableNameReactive().collectList().block(TIMEOUT);
            System.out.println("[表名枚举] 模式=" + mode() + "，实际=" + tableNames);
            Assert.assertNotNull(tableNames);
            Assert.assertEquals(2, tableNames.size());
            Assert.assertTrue(tableNames.contains(firstTableName));
            Assert.assertTrue(tableNames.contains(secondTableName));

            List<RDBTableMetadata> parsedTables = parser.parseAllReactive()
                .cast(RDBTableMetadata.class)
                .collectList()
                .block(TIMEOUT);
            Assert.assertNotNull(parsedTables);
            Assert.assertEquals(2, parsedTables.size());
            for (RDBTableMetadata parsed : parsedTables) {
                System.out.println("[批量解析结果] 模式=" + mode() + "，表名=" + parsed.getName()
                    + "，实际表名=" + parsed.getRealName() + "，字段数=" + parsed.getColumns().size()
                    + "，索引=" + parsed.getIndexes());
                Assert.assertTrue(tableNames.contains(parsed.getName()));
                Assert.assertEquals(parsed.getName(), parsed.getRealName());
                Assert.assertEquals(firstTableName.equals(parsed.getName()) ? 3 : 2, parsed.getColumns().size());
                Assert.assertEquals(32, parsed.getColumnNow("id").getLength());
                Assert.assertTrue(parsed.getColumnNow("id").isPrimaryKey());
                Assert.assertTrue(parsed.getColumnNow("id").isNotNull());
                Assert.assertEquals(1, parsed.getIndexes().size());
                RDBIndexMetadata primary = parsed.getIndexes().get(0);
                Assert.assertTrue(primary.isPrimaryKey());
                Assert.assertTrue(primary.isUnique());
                Assert.assertEquals(parsed.getName(), primary.getTableName());
                Assert.assertEquals(1, primary.getColumns().size());
                Assert.assertEquals("id", primary.getColumns().get(0).getColumn());
                parsedSchema.addTable(parsed);
            }

            RDBTableMetadata parsed = parsedSchema.getTable(firstTableName, false)
                .orElseThrow(AssertionError::new);
            Assert.assertTrue(parsedSchema.getTable(secondTableName, false).isPresent());
            Assert.assertEquals(firstTableName, parsed.getRealName());
            Assert.assertEquals(128, parsed.getColumnNow("name").getLength());
            Assert.assertTrue(parsed.getColumnNow("name").isNotNull());
            Assert.assertEquals(oracleMode ? JDBCType.NUMERIC : JDBCType.INTEGER,
                parsed.getColumnNow("age").getSqlType());

            // 仅使用从数据库批量解析并注册的元数据执行后续操作。
            DatabaseOperator parsedOperator = DefaultDatabaseOperator.of(parsedDatabase);
            ReactiveRepository<Record, String> repository = parsedOperator.dml()
                .createReactiveRepository(firstTableName);
            Assert.assertEquals(1, repository.save(Record.newRecord()
                .putValue("id", "reverse-engineered")
                .putValue("name", "before")
                .putValue("age", 1)).block(TIMEOUT).getTotal());

            Record loaded = repository.findById("reverse-engineered").block(TIMEOUT);
            Assert.assertNotNull(loaded);
            Assert.assertEquals("before", loaded.getString("name").orElse(null));
            Assert.assertEquals(Integer.valueOf(1), loaded.getInteger("age").orElse(null));
            System.out.println("[反向解析后仓储查询] " + loaded);

            Assert.assertEquals(Integer.valueOf(1), repository.createUpdate()
                .set("name", "after")
                .where("id", "reverse-engineered")
                .execute().block(TIMEOUT));
            List<Record> page = repository.createQuery()
                .orderBy(SortOrder.asc("id"))
                .paging(0, 10)
                .fetch().collectList().block(TIMEOUT);
            Assert.assertNotNull(page);
            Assert.assertEquals(1, page.size());
            Assert.assertEquals("after", page.get(0).getString("name").orElse(null));
            System.out.println("[反向解析后修改与分页] " + page);

            Assert.assertEquals(Integer.valueOf(1), repository.createDelete()
                .where("id", "reverse-engineered")
                .execute().block(TIMEOUT));
            Assert.assertNull(repository.findById("reverse-engineered").block(TIMEOUT));
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + metadataSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，表枚举、两表批量反向解析、小写实际表名、主键标识及解析后的仓储操作均符合预期");
    }

    /**
     * 复用官方注解实体，验证多类型字段经注解映射、R2DBC 写入和实体回读的完整流程。
     */
    @Test
    public void shouldRoundTripAnnotatedEntityTypes() {
        String entitySchemaName = tableName + "_entity";
        String quotedSchema = schema.getDialect().quote(entitySchemaName, false);
        System.out.println("[注解实体多类型] 模式=" + mode() + "，临时 Schema=" + entitySchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            RDBSchemaMetadata entitySchema = createSchema(entitySchemaName);
            RDBDatabaseMetadata entityDatabase = new RDBDatabaseMetadata(entitySchema.getDialect());
            entityDatabase.addSchema(entitySchema);
            entityDatabase.setCurrentSchema(entitySchema);
            entityDatabase.addFeature(executor);
            DatabaseOperator entityOperator = DefaultDatabaseOperator.of(entityDatabase);

            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(entityOperator);
            entityOperator.dml().createReactiveRepository("test_address")
                .insert(Record.newRecord().putValue("id", "address").putValue("name", "test_address"))
                .block(TIMEOUT);

            BasicTestEntity entity = BasicTestEntity.builder()
                .id("annotated-types")
                .name("Kingbase实体")
                .createTime(new Date(Timestamp.valueOf("2026-07-27 10:30:15").getTime()))
                .state((byte) 1)
                .balance(9_000_000_000L)
                .floatVal(1.25F)
                .doubleVal(9.5D)
                .bigDecimal(new BigDecimal("1.2"))
                .enabled(true)
                .stateEnum(StateEnum.enabled)
                .stateEnums(new StateEnum[]{StateEnum.enabled, StateEnum.disabled})
                .tags(Arrays.asList("a", "b", "中文"))
                .addressId("address")
                .build();
            System.out.println("[实体写入] " + entity);
            Assert.assertEquals(Integer.valueOf(1), repository.insert(entity).block(TIMEOUT));
            BasicTestEntity inserted = repository.findById(entity.getId()).block(TIMEOUT);
            assertAnnotatedEntityValues(entity, inserted);
            Assert.assertNotNull(inserted.getAddress());
            Assert.assertEquals(entity.getAddressId(), inserted.getAddress().getId());
            Assert.assertEquals("test_address", inserted.getAddress().getName());
            System.out.println("[关联实体回读] id=" + inserted.getAddress().getId()
                + "，name=" + inserted.getAddress().getName());
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery().count().block(TIMEOUT));

            entity.setName("Kingbase实体更新");
            entity.setState((byte) 2);
            entity.setBalance(10_000_000_000L);
            entity.setFloatVal(2.5F);
            entity.setDoubleVal(9.75D);
            entity.setBigDecimal(new BigDecimal("2.3"));
            entity.setEnabled(false);
            entity.setStateEnum(StateEnum.disabled);
            entity.setStateEnums(new StateEnum[]{StateEnum.warn});
            entity.setTags(Arrays.asList("updated", "中文"));
            Assert.assertEquals(Integer.valueOf(1), repository.updateById(entity.getId(), Mono.just(entity))
                .block(TIMEOUT));
            assertAnnotatedEntityValues(entity, repository.findById(entity.getId()).block(TIMEOUT));

            entity.setName("Kingbase实体保存");
            Assert.assertEquals(1, repository.save(entity).block(TIMEOUT).getTotal());
            BasicTestEntity saved = repository.createQuery()
                .select("*")
                .where(BasicTestEntity::getId, entity.getId())
                .orderBy(SortOrder.asc("id"))
                .fetchOne().block(TIMEOUT);
            assertAnnotatedEntityValues(entity, saved);

            Assert.assertEquals(Integer.valueOf(1), repository.deleteById(entity.getId()).block(TIMEOUT));
            Assert.assertNull(repository.findById(entity.getId()).block(TIMEOUT));
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + entitySchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，官方注解实体的多类型写入、更新、保存、回读和删除均符合预期");
    }

    /**
     * 对应官方 testJoin/testRepositoryCurd，验证关联回读、嵌套条件和关联条件更新、删除。
     */
    @Test
    public void shouldExecuteAnnotatedRepositoryAndAssociationOperations() {
        String entitySchemaName = tableName + "_join";
        String quotedSchema = schema.getDialect().quote(entitySchemaName, false);
        System.out.println("[实体仓储与关联操作] 模式=" + mode() + "，临时 Schema=" + entitySchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            RDBSchemaMetadata entitySchema = createSchemaWithExecutor(entitySchemaName);
            DatabaseOperator entityOperator = DefaultDatabaseOperator.of(entitySchema.getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(entityOperator);
            ReactiveRepository<Record, String> addresses = entityOperator.dml().createReactiveRepository("test_address");
            Assert.assertEquals(Integer.valueOf(2), addresses.insert(Flux.just(
                Record.newRecord().putValue("id", "addr-joined").putValue("name", "test_address"),
                Record.newRecord().putValue("id", "addr-other").putValue("name", "other_address")
            )).block(TIMEOUT));
            BasicTestEntity joined = BasicTestEntity.builder()
                .id("joined").name("target").state((byte) 1).addressId("addr-joined")
                .stateEnum(StateEnum.enabled).stateEnums(new StateEnum[]{StateEnum.enabled}).enabled(true).build();
            BasicTestEntity other = BasicTestEntity.builder()
                .id("untouched").name("control").state((byte) 1).addressId("addr-other")
                .stateEnum(StateEnum.enabled).stateEnums(new StateEnum[]{StateEnum.enabled}).enabled(false).build();
            Assert.assertEquals(Integer.valueOf(2), repository.insert(Flux.just(joined, other)).block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(2), repository.createQuery().count().block(TIMEOUT));

            BasicTestEntity loaded = repository.findById(joined.getId()).block(TIMEOUT);
            Assert.assertNotNull(loaded);
            Assert.assertEquals(joined.getId(), loaded.getId());
            Assert.assertEquals("target", loaded.getName());
            Assert.assertNotNull(loaded.getAddress());
            Assert.assertEquals("addr-joined", loaded.getAddress().getId());
            Assert.assertEquals("test_address", loaded.getAddress().getName());
            System.out.println("[关联对象回读] id=" + loaded.getId() + "，address.id="
                + loaded.getAddress().getId() + "，address.name=" + loaded.getAddress().getName());

            // 复现官方 getter 条件链：根条件 AND (根字段条件 OR 关联字段条件)。
            List<BasicTestEntity> officialSelected = repository.createQuery()
                .selectExcludes("address.*")
                .where(joined::getId)
                .nest().is(joined::getId).or().is("address.name", "test").end()
                .orderBy(SortOrder.desc("id"))
                .paging(0, 10)
                .fetch().collectList().block(TIMEOUT);
            Assert.assertNotNull(officialSelected);
            Assert.assertEquals(1, officialSelected.size());
            Assert.assertEquals(joined, officialSelected.get(0));
            Assert.assertNull(officialSelected.get(0).getAddress());
            System.out.println("[官方关联条件链] getter 根条件和嵌套 OR 命中目标实体，业务字段一致，关联投影已排除");

            // id=missing 不匹配，必须由 OR 中的 address.name 条件选中目标记录。
            List<BasicTestEntity> selected = repository.createQuery()
                .selectExcludes("address.*")
                .where("state", (byte) 1)
                .nest().is("id", "missing").or().is("address.name", "test_address").end()
                .orderBy(SortOrder.desc("id"))
                .paging(0, 10)
                .fetch().collectList().block(TIMEOUT);
            Assert.assertNotNull(selected);
            Assert.assertEquals(1, selected.size());
            Assert.assertEquals(joined.getId(), selected.get(0).getId());
            Assert.assertEquals("target", selected.get(0).getName());
            Assert.assertNull(selected.get(0).getAddress());
            System.out.println("[关联嵌套查询] 命中 id=" + selected.get(0).getId()
                + "，排序分页正确，排除 address.* 后关联对象为空");

            // 若丢失括号，关联 OR 条件会错误地放入 untouched 对照记录。
            List<BasicTestEntity> noMatch = repository.createQuery()
                .selectExcludes("address.*")
                .where(joined::getId)
                .nest().is("id", "missing").or().is("address.name", "other_address").end()
                .fetch().collectList().block(TIMEOUT);
            Assert.assertNotNull(noMatch);
            Assert.assertTrue(noMatch.isEmpty());
            System.out.println("[嵌套条件优先级] 根条件 AND (不匹配 ID OR 对照关联条件) 返回空结果，未泄漏对照记录");

            joined.setState((byte) 2);
            Integer updated = repository.createUpdate()
                .set(joined::getState)
                .nest().is(joined::getId).or().is("id", "missing").end()
                .where(joined::getId).and().is("address.name", "test_address")
                .execute().block(TIMEOUT);
            Assert.assertEquals(Integer.valueOf(1), updated);
            Assert.assertEquals(Byte.valueOf((byte) 2), repository.findById(joined.getId()).block(TIMEOUT).getState());
            Assert.assertEquals(Byte.valueOf((byte) 1), repository.findById(other.getId()).block(TIMEOUT).getState());
            Assert.assertEquals(Integer.valueOf(0), repository.createUpdate()
                .set("state", (byte) 9)
                .where("id", other.getId()).and().is("address.name", "test_address")
                .execute().block(TIMEOUT));
            Assert.assertEquals(Byte.valueOf((byte) 1), repository.findById(other.getId()).block(TIMEOUT).getState());
            System.out.println("[关联条件更新] 目标记录影响 1 行，state=2；不匹配关联条件影响 0 行，对照记录未修改");

            Integer deleted = repository.createDelete()
                .where(joined::getId)
                .nest().is(joined::getId).or().is("id", "missing").end()
                .is("address.name", "test_address")
                .execute().block(TIMEOUT);
            Assert.assertEquals(Integer.valueOf(1), deleted);
            Assert.assertNull(repository.findById(joined.getId()).block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(0), repository.createDelete()
                .where("id", other.getId()).and().is("address.name", "test_address")
                .execute().block(TIMEOUT));
            BasicTestEntity remaining = repository.findById(other.getId()).block(TIMEOUT);
            Assert.assertNotNull(remaining);
            Assert.assertEquals(Byte.valueOf((byte) 1), remaining.getState());
            Assert.assertNotNull(remaining.getAddress());
            Assert.assertEquals("other_address", remaining.getAddress().getName());
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery().count().block(TIMEOUT));
            System.out.println("[关联条件删除] 目标记录删除 1 行，不匹配关联条件删除 0 行，对照记录及关联对象保留");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + entitySchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，实体仓储、关联回读、嵌套查询、字段排除、排序分页及关联条件更新/删除均符合预期");
    }

    /**
     * 对应官方实体流插入、分批插入及未指定主键的批量插入场景。
     */
    @Test
    public void shouldInsertEntityStreamsInBatches() {
        String batchSchemaName = tableName + "_batch";
        String quotedSchema = schema.getDialect().quote(batchSchemaName, false);
        System.out.println("[实体流分批插入] 模式=" + mode() + "，临时 Schema=" + batchSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator batchOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(batchSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(batchOperator);
            seedRepositoryAddress(batchOperator);

            Integer streamed = repository.insert(Flux.range(0, 10)
                .map(i -> repositoryEntity("stream-" + i, "stream:" + i))).block(TIMEOUT);
            Assert.assertEquals(Integer.valueOf(10), streamed);
            Assert.assertEquals(Integer.valueOf(10), repository.createQuery().count().block(TIMEOUT));
            Assert.assertEquals("stream:0", repository.findById("stream-0").block(TIMEOUT).getName());
            Assert.assertEquals("stream:9", repository.findById("stream-9").block(TIMEOUT).getName());
            System.out.println("[实体流插入] 预期 10 条，实际影响=" + streamed + "，首尾记录回读正确");

            Integer batched = repository.insertBatch(Flux.range(0, 100)
                .map(i -> repositoryEntity("batch-" + i, "batch:" + i))
                .buffer(10)
                .delayElements(Duration.ofMillis(100))
                .doOnNext(batch -> {
                    Assert.assertEquals(10, batch.size());
                    System.out.println("[分批插入输入] 本批=" + batch.size() + "，首条=" + batch.get(0).getId());
                })).block(TIMEOUT);
            Assert.assertEquals(Integer.valueOf(100), batched);
            Assert.assertEquals(Integer.valueOf(100), repository.createQuery().like$("id", "batch-").count().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(110), repository.createQuery().count().block(TIMEOUT));
            Assert.assertEquals("batch:0", repository.findById("batch-0").block(TIMEOUT).getName());
            Assert.assertEquals("batch:99", repository.findById("batch-99").block(TIMEOUT).getName());
            System.out.println("[分批插入结果] 10 批共 100 条，实际影响=" + batched + "，累计记录=110");

            List<BasicTestEntity> generated = Flux.range(0, 100)
                .map(i -> repositoryEntity(null, "generated:" + i))
                .collectList().block(TIMEOUT);
            Assert.assertNotNull(generated);
            Assert.assertEquals(Integer.valueOf(100), repository.insertBatch(generated).block(TIMEOUT));
            for (BasicTestEntity entity : generated) {
                Assert.assertNotNull(entity.getId());
                Assert.assertEquals(32, entity.getId().length());
            }
            Map<String, String> expected = generated.stream()
                .collect(Collectors.toMap(BasicTestEntity::getId, BasicTestEntity::getName));
            Assert.assertEquals(100, expected.size());
            List<BasicTestEntity> reloaded = repository.createQuery()
                .select("id", "name").like$("name", "generated:")
                .fetch().collectList().block(TIMEOUT);
            Assert.assertNotNull(reloaded);
            Assert.assertEquals(100, reloaded.size());
            Assert.assertEquals(expected, reloaded.stream()
                .collect(Collectors.toMap(BasicTestEntity::getId, BasicTestEntity::getName)));
            Assert.assertEquals(Integer.valueOf(210), repository.createQuery().count().block(TIMEOUT));
            System.out.println("[自动主键批量插入] 100 条主键均生成并回填、互不重复，全部 ID/名称实库回读一致，累计记录=210");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + batchSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode() + "，实体流、分批插入及自动主键批量插入均符合预期");
    }

    /**
     * 对应官方 testInsertMerge/testInsertDuplicate，区分批次内合并与已存在数据的重复键异常。
     */
    @Test
    public void shouldMergeDuplicateEntityPrimaryKeysWithinBatch() {
        String mergeSchemaName = tableName + "_merge";
        String quotedSchema = schema.getDialect().quote(mergeSchemaName, false);
        System.out.println("[批次内重复主键合并] 模式=" + mode() + "，临时 Schema=" + mergeSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator mergeOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(mergeSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(mergeOperator);
            seedRepositoryAddress(mergeOperator);

            BasicTestEntity first = repositoryEntity("test_merge", "first");
            first.setDoubleVal(1D);
            BasicTestEntity second = repositoryEntity("test_merge", "second");
            second.setBalance(null);
            second.setExtension("ext_name", "test");
            Assert.assertEquals(Integer.valueOf(1), repository.insert(Flux.just(first, second)).block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery().count().block(TIMEOUT));
            BasicTestEntity merged = repository.createQuery()
                .select("id", "name", "balance", "doubleVal", "ext_name")
                .where("id", "test_merge").fetchOne().block(TIMEOUT);
            Assert.assertNotNull(merged);
            Assert.assertEquals("second", merged.getName());
            Assert.assertEquals(Long.valueOf(1000), merged.getBalance());
            Assert.assertEquals(Double.valueOf(1D), merged.getDoubleVal());
            Assert.assertEquals("test", merged.getExtension("ext_name"));
            System.out.println("[同主键不同值合并] 2 条输入影响 1 行，name=second，旧 balance/doubleVal 保留，ext_name=test");

            int[] insertIds = {1, 2, 2, 3, 1, 3};
            List<BasicTestEntity> inserts = new ArrayList<>();
            for (int i = 0; i < insertIds.length; i++) {
                inserts.add(repositoryEntity("dup_insert_" + insertIds[i], "insert-" + i));
            }
            Assert.assertEquals(Integer.valueOf(3), repository.insertBatch(inserts).block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(3), repository.createQuery().like$("id", "dup_insert_").count().block(TIMEOUT));
            Assert.assertEquals("insert-4", repository.findById("dup_insert_1").block(TIMEOUT).getName());
            Assert.assertEquals("insert-2", repository.findById("dup_insert_2").block(TIMEOUT).getName());
            Assert.assertEquals("insert-5", repository.findById("dup_insert_3").block(TIMEOUT).getName());
            Assert.assertEquals(Integer.valueOf(3), repository.createDelete().like$("id", "dup_insert_").execute().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(0), repository.createQuery().like$("id", "dup_insert_").count().block(TIMEOUT));
            System.out.println("[insertBatch 去重] 6 条输入写入 3 行，最后一次非空值生效，删除影响 3 行");

            int[] saveIds = {1, 2, 2, 3, 3, 1};
            List<BasicTestEntity> saves = new ArrayList<>();
            for (int i = 0; i < saveIds.length; i++) {
                saves.add(repositoryEntity("dup_save_" + saveIds[i], "save-" + i));
            }
            SaveResult saved = repository.save(Flux.fromIterable(saves)).block(TIMEOUT);
            Assert.assertNotNull(saved);
            Assert.assertEquals(3, saved.getTotal());
            Assert.assertEquals(Integer.valueOf(3), repository.createQuery().like$("id", "dup_save_").count().block(TIMEOUT));
            Assert.assertEquals("save-5", repository.findById("dup_save_1").block(TIMEOUT).getName());
            Assert.assertEquals("save-2", repository.findById("dup_save_2").block(TIMEOUT).getName());
            Assert.assertEquals("save-4", repository.findById("dup_save_3").block(TIMEOUT).getName());
            Assert.assertEquals(Integer.valueOf(3), repository.createDelete().like$("id", "dup_save_").execute().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery().count().block(TIMEOUT));
            System.out.println("[save 去重] 6 条输入保存 3 行，结果=" + saved + "，最后一次非空值生效，删除影响 3 行");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + mergeSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode() + "，批次内主键去重、值合并、扩展字段及影响行数均符合预期");
    }

    /**
     * 对应官方 testRepositorySave/testReactiveRepositorySave/testSaveNull 的保存语义。
     */
    @Test
    public void shouldPreserveEntitySaveSemantics() {
        String saveSchemaName = tableName + "_save";
        String quotedSchema = schema.getDialect().quote(saveSchemaName, false);
        System.out.println("[实体保存语义] 模式=" + mode() + "，临时 Schema=" + saveSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator saveOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(saveSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(saveOperator);
            seedRepositoryAddress(saveOperator);

            BasicTestEntity existing = repositoryEntity("save-existing", "first");
            existing.setDoubleVal(1D);
            SaveResult inserted = repository.save(Mono.just(existing)).block(TIMEOUT);
            Assert.assertNotNull(inserted);
            Assert.assertEquals(1, inserted.getTotal());
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery().count().block(TIMEOUT));
            BasicTestEntity initial = repository.findById(existing.getId()).block(TIMEOUT);
            Assert.assertNotNull(initial);
            Assert.assertEquals("first", initial.getName());
            Assert.assertEquals(Double.valueOf(1D), initial.getDoubleVal());
            Assert.assertEquals(StateEnum.enabled, initial.getStateEnum());
            System.out.println("[save 新增] " + inserted + "，回读=" + initial);

            existing.setName("updated");
            existing.setDoubleVal(2D);
            // 仅修改 Java 对象不会自动写入数据库，调用 save 后才应生效。
            Assert.assertEquals("first", repository.findById(existing.getId()).block(TIMEOUT).getName());
            SaveResult updated = repository.save(existing).block(TIMEOUT);
            Assert.assertNotNull(updated);
            Assert.assertEquals(1, updated.getTotal());
            BasicTestEntity afterUpdate = repository.findById(existing.getId()).block(TIMEOUT);
            Assert.assertNotNull(afterUpdate);
            Assert.assertEquals("updated", afterUpdate.getName());
            Assert.assertEquals(Double.valueOf(2D), afterUpdate.getDoubleVal());
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery().count().block(TIMEOUT));
            System.out.println("[save 更新] " + updated + "，name=updated，doubleVal=2，记录数仍为 1");

            existing.setStateEnum(null);
            SaveResult nullEnum = repository.save(existing).block(TIMEOUT);
            Assert.assertNotNull(nullEnum);
            Assert.assertEquals(1, nullEnum.getTotal());
            BasicTestEntity afterNullEnum = repository.findById(existing.getId()).block(TIMEOUT);
            Assert.assertNotNull(afterNullEnum);
            Assert.assertEquals(StateEnum.enabled, afterNullEnum.getStateEnum());
            Assert.assertEquals(Double.valueOf(2D), afterNullEnum.getDoubleVal());
            System.out.println("[空枚举保存] " + nullEnum + "，数据库 stateEnum 仍为 enabled，未被空值或默认值覆盖");

            BasicTestEntity added = repositoryEntity("save-new", "new");
            added.setDoubleVal(1D);
            BasicTestEntity patch = repositoryEntity(existing.getId(), "mixed");
            Assert.assertNull(patch.getDoubleVal());
            SaveResult mixed = repository.save(Flux.just(added, patch)).block(TIMEOUT);
            Assert.assertNotNull(mixed);
            Assert.assertEquals(2, mixed.getTotal());
            Assert.assertEquals(Integer.valueOf(2), repository.createQuery().count().block(TIMEOUT));
            BasicTestEntity reloadedExisting = repository.findById(existing.getId()).block(TIMEOUT);
            BasicTestEntity reloadedNew = repository.findById(added.getId()).block(TIMEOUT);
            Assert.assertNotNull(reloadedExisting);
            Assert.assertNotNull(reloadedNew);
            Assert.assertEquals("mixed", reloadedExisting.getName());
            Assert.assertEquals(Double.valueOf(2D), reloadedExisting.getDoubleVal());
            Assert.assertEquals(StateEnum.enabled, reloadedExisting.getStateEnum());
            Assert.assertEquals("new", reloadedNew.getName());
            Assert.assertEquals(Double.valueOf(1D), reloadedNew.getDoubleVal());
            System.out.println("[混合新增更新] " + mixed + "，已有记录 name=mixed、doubleVal=2 保留，新增记录 doubleVal=1");

            // 对应官方保存测试中的指定值及多字段排序，并使用两条记录明确校验顺序。
            List<BasicTestEntity> ordered = repository.createQuery().select("*")
                .orderBy(SortOrder.desc("id").value(existing.getId()),
                    SortOrder.desc("id").value(added.getId()),
                    SortOrder.desc("createTime"), SortOrder.desc("state"))
                .fetch().collectList().block(TIMEOUT);
            Assert.assertNotNull(ordered);
            Assert.assertEquals(2, ordered.size());
            Assert.assertEquals(existing.getId(), ordered.get(0).getId());
            Assert.assertEquals(added.getId(), ordered.get(1).getId());
            System.out.println("[保存后指定值排序] 实际 ID 顺序=" + ordered.stream()
                .map(BasicTestEntity::getId).collect(Collectors.toList()));
            // added/updated 分项计数按官方契约不保证准确，只严格校验 total 和实际数据。
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + saveSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，save 新增、更新、混合保存、空值保留、总数及指定值排序均符合预期");
    }

    /**
     * 对应官方 testEnums：普通 in 匹配完整枚举掩码，ANY 匹配其中任意一位。
     */
    @Test
    public void shouldMatchAnnotatedEnumMasksWithInAndAnyConditions() {
        String enumSchemaName = tableName + "_enums";
        String quotedSchema = schema.getDialect().quote(enumSchemaName, false);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator enumOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(enumSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(enumOperator);
            BasicTestEntity entity = repositoryEntity("enums_id", "test");
            entity.setStateEnums(new StateEnum[]{StateEnum.enabled, StateEnum.disabled});
            Assert.assertEquals(Integer.valueOf(1), repository.insert(entity).block(TIMEOUT));

            Assert.assertTrue(repository.createQuery().select("id", "stateEnums")
                .in(BasicTestEntity::getStateEnums, StateEnum.enabled)
                .fetch().collectList().block(TIMEOUT).isEmpty());
            List<BasicTestEntity> completeMask = repository.createQuery().select("id", "stateEnums")
                .in(BasicTestEntity::getStateEnums, StateEnum.enabled, StateEnum.disabled)
                .fetch().collectList().block(TIMEOUT);
            Assert.assertEquals(1, completeMask.size());
            Assert.assertEquals(entity.getId(), completeMask.get(0).getId());
            Assert.assertArrayEquals(entity.getStateEnums(), completeMask.get(0).getStateEnums());
            System.out.println("[普通枚举匹配] 模式=" + mode() + "，单个 enabled 不匹配组合掩码，enabled+disabled 匹配且正确回读");

            List<BasicTestEntity> any = repository.createQuery().select("id", "stateEnums")
                .where(Terms.enumInAny(BasicTestEntity::getStateEnums, StateEnum.disabled))
                .fetch().collectList().block(TIMEOUT);
            Assert.assertEquals(1, any.size());
            Assert.assertEquals(entity.getId(), any.get(0).getId());
            Assert.assertArrayEquals(entity.getStateEnums(), any.get(0).getStateEnums());
            Assert.assertTrue(repository.createQuery().select("id", "stateEnums")
                .where(Terms.enumInAny(BasicTestEntity::getStateEnums, StateEnum.warn))
                .fetch().collectList().block(TIMEOUT).isEmpty());
            Assert.assertEquals(Integer.valueOf(1), repository.createQuery()
                .where(Terms.enumNotInAny(BasicTestEntity::getStateEnums, StateEnum.warn)).count().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(0), repository.createQuery()
                .where(Terms.enumNotInAny(BasicTestEntity::getStateEnums, StateEnum.enabled)).count().block(TIMEOUT));
            System.out.println("[枚举 ANY 条件] 模式=" + mode() + "，disabled 命中、warn 不命中；NOT ANY warn 命中、enabled 不命中");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + enumSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode() + "，官方普通枚举及 ANY 条件均通过");
    }

    /**
     * 对应官方 testNotOrNULL：否定条件应包含字段为 NULL 的记录。
     */
    @Test
    public void shouldIncludeNullValuesInNegatedConditions() {
        String nullSchemaName = tableName + "_nulls";
        String quotedSchema = schema.getDialect().quote(nullSchemaName, false);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator nullOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(nullSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(nullOperator);
            BasicTestEntity nullAddress = repositoryEntity("nullAddressId", "name");
            nullAddress.setAddressId(null);
            BasicTestEntity hasAddress = repositoryEntity("hasAddressId", "name");
            hasAddress.setAddressId("0");
            Assert.assertEquals(Integer.valueOf(2), repository.insert(Arrays.asList(nullAddress, hasAddress)).block(TIMEOUT));

            Assert.assertEquals(Integer.valueOf(2), repository.createQuery()
                .notLike(BasicTestEntity::getAddressId, "no").count().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(2), repository.createQuery()
                .not(BasicTestEntity::getAddressId, "no").count().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(2), repository.createQuery()
                .notIn(BasicTestEntity::getAddressId, "no").count().block(TIMEOUT));
            Assert.assertEquals(Integer.valueOf(2), repository.createQuery()
                .notBetween(BasicTestEntity::getAddressId, "1", "3").count().block(TIMEOUT));
            System.out.println("[否定条件包含空值] 模式=" + mode()
                + "，notLike/no、not/no、notIn/no、notBetween/1..3 均返回 2，包含 NULL 和 0");

            // 排除非空记录后仍必须准确保留 NULL，防止只验证总数而漏掉条件失效。
            List<String> notLike = repository.createQuery().select("id")
                .notLike(BasicTestEntity::getAddressId, "0").fetch()
                .map(BasicTestEntity::getId).collectList().block(TIMEOUT);
            List<String> not = repository.createQuery().select("id")
                .not(BasicTestEntity::getAddressId, "0").fetch()
                .map(BasicTestEntity::getId).collectList().block(TIMEOUT);
            List<String> notIn = repository.createQuery().select("id")
                .notIn(BasicTestEntity::getAddressId, "0").fetch()
                .map(BasicTestEntity::getId).collectList().block(TIMEOUT);
            List<String> notBetween = repository.createQuery().select("id")
                .notBetween(BasicTestEntity::getAddressId, "0", "1").fetch()
                .map(BasicTestEntity::getId).collectList().block(TIMEOUT);
            List<String> expected = Arrays.asList(nullAddress.getId());
            Assert.assertEquals(expected, notLike);
            Assert.assertEquals(expected, not);
            Assert.assertEquals(expected, notIn);
            Assert.assertEquals(expected, notBetween);
            System.out.println("[否定条件对照] 模式=" + mode() + "，四种条件排除非空值 0 后均仅返回 " + expected);
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + nullSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode() + "，官方否定条件包含 NULL 场景通过");
    }

    /**
     * 对应官方 testUpdateByNative，同时在 SET 和比较条件中使用 NativeSql。
     */
    @Test
    public void shouldUseNativeExpressionsInUpdateAndComparisonConditions() {
        String expressionSchemaName = tableName + "_expr";
        String quotedSchema = schema.getDialect().quote(expressionSchemaName, false);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator expressionOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(expressionSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(expressionOperator);
            seedRepositoryAddress(expressionOperator);
            BasicTestEntity target = repositoryEntity("testUpdateByNative", "target");
            BasicTestEntity control = repositoryEntity("expression-control", "control");
            control.setState((byte) 3);
            Assert.assertEquals(Integer.valueOf(2), repository.insert(Arrays.asList(target, control)).block(TIMEOUT));

            Integer updated = repository.createUpdate()
                .set(BasicTestEntity::getState, NativeSql.of("state + 1"))
                .where(target::getId)
                .lte(BasicTestEntity::getState, NativeSql.of("state + 1"))
                .execute().block(TIMEOUT);
            Assert.assertEquals(Integer.valueOf(1), updated);
            Assert.assertEquals(Byte.valueOf((byte) 2), repository.findById(target.getId()).block(TIMEOUT).getState());
            Assert.assertEquals(Byte.valueOf((byte) 3), repository.findById(control.getId()).block(TIMEOUT).getState());
            System.out.println("[表达式更新] 模式=" + mode()
                + "，SET state=state+1 且 state<=state+1，影响 1 行，目标 1→2，对照仍为 3");

            Integer unmatched = repository.createUpdate()
                .set(BasicTestEntity::getState, NativeSql.of("state + 1"))
                .where(target::getId)
                .lte(BasicTestEntity::getState, NativeSql.of("state - 1"))
                .execute().block(TIMEOUT);
            Assert.assertEquals(Integer.valueOf(0), unmatched);
            Assert.assertEquals(Byte.valueOf((byte) 2), repository.findById(target.getId()).block(TIMEOUT).getState());
            Assert.assertEquals(Byte.valueOf((byte) 3), repository.findById(control.getId()).block(TIMEOUT).getState());
            System.out.println("[表达式条件对照] 模式=" + mode()
                + "，state<=state-1 不成立，影响 0 行，目标及对照均保持原值");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + expressionSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode() + "，官方 SET 及比较条件中的原生 SQL 表达式通过");
    }

    /**
     * 对应官方 testReactiveRepositorySave 的指定值及多字段排序，用五条记录校验完整顺序。
     */
    @Test
    public void shouldOrderBySpecifiedValuesThenMultipleColumns() {
        String sortSchemaName = tableName + "_sort";
        String quotedSchema = schema.getDialect().quote(sortSchemaName, false);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            DatabaseOperator sortOperator = DefaultDatabaseOperator.of(createSchemaWithExecutor(sortSchemaName).getDatabase());
            ReactiveRepository<BasicTestEntity, String> repository = createAnnotatedEntityRepository(sortOperator);
            seedRepositoryAddress(sortOperator);
            BasicTestEntity first = repositoryEntity("pin-b", "first specified value");
            BasicTestEntity second = repositoryEntity("pin-a", "second specified value");
            BasicTestEntity older = repositoryEntity("other-z", "older but higher state");
            BasicTestEntity recentLow = repositoryEntity("other-y", "recent lower state");
            BasicTestEntity recentHigh = repositoryEntity("other-x", "recent higher state");
            older.setState((byte) 9);
            recentLow.setState((byte) 1);
            recentHigh.setState((byte) 2);
            Date recent = new Date(Timestamp.valueOf("2026-08-01 10:30:15").getTime());
            recentLow.setCreateTime(recent);
            recentHigh.setCreateTime(recent);
            // 打乱输入顺序，使预期结果不能由插入顺序或普通 ID 排序碰巧满足。
            Assert.assertEquals(Integer.valueOf(5), repository
                .insert(Arrays.asList(second, older, recentLow, first, recentHigh)).block(TIMEOUT));

            List<String> descending = repository.createQuery().select("*")
                .orderBy(SortOrder.desc("id").value(first.getId()),
                    SortOrder.desc("id").value(second.getId()),
                    SortOrder.desc("createTime"), SortOrder.desc("state"))
                .fetch().map(BasicTestEntity::getId).collectList().block(TIMEOUT);
            Assert.assertEquals(Arrays.asList(recentHigh.getId(), recentLow.getId(), older.getId(),
                first.getId(), second.getId()), descending);
            System.out.println("[官方指定值排序] 模式=" + mode() + "，desc(value) 指定值排后，实际顺序=" + descending);

            List<String> ascending = repository.createQuery().select("*")
                .orderBy(SortOrder.asc("id").value(first.getId()),
                    SortOrder.asc("id").value(second.getId()),
                    SortOrder.desc("createTime"), SortOrder.desc("state"))
                .fetch().map(BasicTestEntity::getId).collectList().block(TIMEOUT);
            Assert.assertEquals(Arrays.asList(first.getId(), second.getId(), recentHigh.getId(),
                recentLow.getId(), older.getId()), ascending);
            System.out.println("[指定值排前] 模式=" + mode() + "，asc(value) 指定值排前，实际顺序=" + ascending);
            System.out.println("[后续排序字段] 模式=" + mode()
                + "，未指定的三条记录按 createTime 降序、同时间按 state 降序，state=9 的旧记录仍排在新记录后");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + sortSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode() + "，官方指定值及多字段排序通过");
    }

    private BasicTestEntity repositoryEntity(String id, String name) {
        return BasicTestEntity.builder().id(id).name(name).state((byte) 1).balance(1000L)
            .createTime(new Date(Timestamp.valueOf("2026-07-27 10:30:15").getTime()))
            .tags(Arrays.asList("a", "b", "c", "d"))
            .stateEnum(StateEnum.enabled).stateEnums(new StateEnum[]{StateEnum.enabled})
            .enabled(true).addressId("address").build();
    }

    private void seedRepositoryAddress(DatabaseOperator entityOperator) {
        Assert.assertEquals(Integer.valueOf(1), entityOperator.dml().createReactiveRepository("test_address")
            .insert(Record.newRecord().putValue("id", "address").putValue("name", "test_address"))
            .block(TIMEOUT));
    }

    private ReactiveRepository<BasicTestEntity, String> createAnnotatedEntityRepository(DatabaseOperator entityOperator) {
        RDBDatabaseMetadata database = entityOperator.getMetadata();
        database.getCurrentSchema().addFeature(new DefaultValueGenerator<RDBColumnMetadata>() {
            @Override
            public String getSortId() {
                return "uuid";
            }

            @Override
            public RuntimeDefaultValue generate(RDBColumnMetadata column) {
                return () -> UUID.randomUUID().toString().replace("-", "");
            }

            @Override
            public String getName() {
                return "UUID";
            }
        });
        // 复用官方注解解析器和 BasicCommonTests 的嵌套实体回读包装器。
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(database);
        RDBTableMetadata addressTable = parser.parseTableMetadata(Address.class).orElseThrow(AssertionError::new);
        entityOperator.ddl().createOrAlter(addressTable).commit().reactive().block(TIMEOUT);
        RDBTableMetadata entityTable = parser.parseTableMetadata(BasicTestEntity.class).orElseThrow(AssertionError::new);
        for (RDBColumnMetadata column : entityTable.getColumns()) {
            System.out.println("[注解字段映射] 模式=" + mode() + "，字段=" + column.getName()
                + "，属性=" + column.getAlias() + "，JDBC=" + column.getSqlType()
                + "，数据库类型=" + column.getDataType());
        }
        entityOperator.ddl().createOrAlter(entityTable)
            .addColumn("ext_name").varchar(32).commit()
            .commit().reactive().block(TIMEOUT);
        EntityColumnMapping mapping = entityTable
            .getFeatureNow(MappingFeatureType.columnPropertyMapping.createFeatureId(BasicTestEntity.class));
        mapping.reload();
        return new DefaultReactiveRepository<>(entityOperator, entityTable, BasicTestEntity.class,
            new NestedEntityResultWrapper<BasicTestEntity>(mapping));
    }

    private void assertAnnotatedEntityValues(BasicTestEntity expected, BasicTestEntity actual) {
        Assert.assertNotNull(actual);
        System.out.println("[实体实际回读] 模式=" + mode() + "，" + actual);
        Assert.assertEquals("id", expected.getId(), actual.getId());
        Assert.assertEquals("String/name", expected.getName(), actual.getName());
        Assert.assertNotNull(actual.getCreateTime());
        Assert.assertEquals("Date/createTime", expected.getCreateTime().getTime(), actual.getCreateTime().getTime());
        Assert.assertEquals("Byte/state", expected.getState(), actual.getState());
        Assert.assertEquals("Long/balance", expected.getBalance(), actual.getBalance());
        Assert.assertEquals("Float/floatVal", expected.getFloatVal(), actual.getFloatVal());
        Assert.assertEquals("Double/doubleVal", expected.getDoubleVal(), actual.getDoubleVal());
        Assert.assertEquals("BigDecimal/bigDecimal", expected.getBigDecimal(), actual.getBigDecimal());
        Assert.assertEquals("Boolean/enabled", expected.getEnabled(), actual.getEnabled());
        Assert.assertEquals("Enum/stateEnum", expected.getStateEnum(), actual.getStateEnum());
        Assert.assertArrayEquals("Enum[]/stateEnums", expected.getStateEnums(), actual.getStateEnums());
        Assert.assertEquals("JSON List/tags", expected.getTags(), actual.getTags());
        Assert.assertEquals("String/addressId", expected.getAddressId(), actual.getAddressId());
        System.out.println("[字段校验通过] 模式=" + mode()
            + "，字符串、Byte、Long、Float、Double、BigDecimal、Boolean、日期、枚举、枚举数组及 JSON 列表一致");
    }

    /**
     * 覆盖官方 testDDL 的建表类型集合，按兼容模式验证生成类型并执行真实 DDL。
     */
    @Test
    public void shouldCreateTableWithAllOfficialDdlTypes() {
        boolean oracleMode = "oracle".equals(mode());
        String ddlSchemaName = tableName + "_ddl";
        String quotedSchema = schema.getDialect().quote(ddlSchemaName, false);
        String ddlTableName = "test_ddl_create";
        Map<String, String> expectedTypes = new LinkedHashMap<>();
        expectedTypes.put("id", oracleMode ? "varchar2(32 char)" : "varchar(32)");
        expectedTypes.put("name", oracleMode ? "varchar2(64 char)" : "varchar(64)");
        expectedTypes.put("comment", oracleMode ? "varchar2(32 char)" : "varchar(32)");
        expectedTypes.put("number_test", oracleMode ? "number(4,0)" : "decimal(4,0)");
        expectedTypes.put("datetime_test", oracleMode ? "timestamp" : "datetime(0)");
        expectedTypes.put("int_test", oracleMode ? "number(20)" : "int");
        expectedTypes.put("bigint_test", oracleMode ? "number(38,0)" : "bigint");
        expectedTypes.put("clob_test", oracleMode ? "clob" : "text");
        expectedTypes.put("tinyint_test", oracleMode ? "number(1)" : "tinyint");
        expectedTypes.put("blob_test", "blob");
        expectedTypes.put("char_test", "char(32)");
        expectedTypes.put("time_test", oracleMode ? "timestamp" : "time");
        expectedTypes.put("date_test", "date");
        Map<String, JDBCType> expectedReadTypes = new LinkedHashMap<>();
        expectedReadTypes.put("id", JDBCType.VARCHAR);
        expectedReadTypes.put("name", JDBCType.VARCHAR);
        expectedReadTypes.put("comment", JDBCType.VARCHAR);
        expectedReadTypes.put("number_test", JDBCType.NUMERIC);
        expectedReadTypes.put("datetime_test", JDBCType.TIMESTAMP);
        expectedReadTypes.put("int_test", oracleMode ? JDBCType.NUMERIC : JDBCType.INTEGER);
        expectedReadTypes.put("bigint_test", oracleMode ? JDBCType.NUMERIC : JDBCType.BIGINT);
        expectedReadTypes.put("clob_test", oracleMode ? JDBCType.LONGVARCHAR : JDBCType.CLOB);
        expectedReadTypes.put("tinyint_test", oracleMode ? JDBCType.NUMERIC : JDBCType.TINYINT);
        expectedReadTypes.put("blob_test", oracleMode ? JDBCType.LONGVARBINARY : JDBCType.BLOB);
        expectedReadTypes.put("char_test", JDBCType.CHAR);
        expectedReadTypes.put("time_test", oracleMode ? JDBCType.TIMESTAMP : JDBCType.TIME);
        expectedReadTypes.put("date_test", oracleMode ? JDBCType.TIMESTAMP : JDBCType.DATE);

        System.out.println("[完整 DDL 类型集合] 模式=" + mode() + "，临时 Schema=" + ddlSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            RDBSchemaMetadata ddlSchema = createSchema(ddlSchemaName);
            RDBDatabaseMetadata ddlDatabase = new RDBDatabaseMetadata(ddlSchema.getDialect());
            ddlDatabase.addSchema(ddlSchema);
            ddlDatabase.setCurrentSchema(ddlSchema);
            ddlDatabase.addFeature(executor);
            DatabaseOperator ddlOperator = DefaultDatabaseOperator.of(ddlDatabase);

            ddlOperator.ddl().createOrAlter(ddlTableName)
                .alias("test").comment("测试")
                .addColumn("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn("name").varchar(64).notNull().comment("名称").commit()
                .addColumn("comment").varchar(32).defaultValueNative("'1'").commit()
                .addColumn("number_test").number(4).commit()
                // 官方将 datetime 和 DATE 复用 date_test，此处拆开以保证两种类型都实际建列。
                .addColumn("datetime_test").datetime().commit()
                .addColumn("int_test").integer().commit()
                .addColumn("bigint_test").bigint().commit()
                .addColumn("clob_test").clob().commit()
                .addColumn("tinyint_test").tinyint().commit()
                .addColumn("blob_test").type(JDBCType.BLOB, byte[].class).commit()
                .addColumn("char_test").type(JDBCType.CHAR, String.class).length(32).commit()
                .addColumn("time_test").type(JDBCType.TIME, LocalTime.class).commit()
                .addColumn("date_test").type(JDBCType.DATE, LocalDate.class).commit()
                .index().name("index_types").column("name").commit()
                .custom(metadata -> {
                    Assert.assertEquals(13, metadata.getColumns().size());
                    for (Map.Entry<String, String> entry : expectedTypes.entrySet()) {
                        RDBColumnMetadata column = metadata.getColumnNow(entry.getKey());
                        String actualType = metadata.getDialect().buildColumnDataType(column);
                        System.out.println("[DDL 类型映射] 模式=" + mode() + "，字段=" + entry.getKey()
                            + "，JDBC=" + column.getSqlType() + "，预期=" + entry.getValue()
                            + "，实际=" + actualType);
                        Assert.assertEquals(entry.getKey(), entry.getValue(), actualType);
                    }
                    SqlRequest request = metadata.findFeatureNow(CreateTableSqlBuilder.ID).build(metadata);
                    System.out.println("[生成建表 SQL] " + request.getSql().replaceAll("\\s+", " ").trim());
                })
                .commit().reactive().block(TIMEOUT);

            // 使用全新的元数据实例回读，证明所有类型都已经在数据库中实际建列。
            RDBSchemaMetadata reloadedSchema = createSchema(ddlSchemaName);
            RDBDatabaseMetadata reloadedDatabase = new RDBDatabaseMetadata(reloadedSchema.getDialect());
            reloadedDatabase.addSchema(reloadedSchema);
            reloadedDatabase.setCurrentSchema(reloadedSchema);
            reloadedDatabase.addFeature(executor);
            RDBTableMetadata reloaded = reloadedSchema.getTableReactive(ddlTableName, true).block(TIMEOUT);
            Assert.assertNotNull(reloaded);
            Assert.assertEquals(13, reloaded.getColumns().size());
            for (String name : expectedTypes.keySet()) {
                RDBColumnMetadata column = reloaded.getColumnNow(name);
                System.out.println("[DDL 实库回读] 模式=" + mode() + "，字段=" + name
                    + "，类型描述=" + column.getDataType() + "，JDBC=" + column.getSqlType()
                    + "，长度=" + column.getLength() + "，精度=" + column.getPrecision()
                    + "，小数位=" + column.getScale());
                Assert.assertEquals(name, expectedReadTypes.get(name), column.getSqlType());
            }
            Assert.assertEquals(32, reloaded.getColumnNow("id").getLength());
            Assert.assertTrue(reloaded.getColumnNow("id").isPrimaryKey());
            Assert.assertTrue(reloaded.getColumnNow("name").isNotNull());
            Assert.assertEquals(64, reloaded.getColumnNow("name").getLength());
            Assert.assertEquals(4, reloaded.getColumnNow("number_test").getPrecision());
            Assert.assertEquals(0, reloaded.getColumnNow("number_test").getScale());
            Assert.assertEquals(32, reloaded.getColumnNow("char_test").getLength());
            Assert.assertTrue(reloaded.getIndexes().stream()
                .anyMatch(index -> "index_types".equals(index.getName())));
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + ddlSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，官方完整 DDL 类型集合生成正确，实库建表成功，13 个字段及索引回读成功");
    }

    /**
     * 对应官方 testDDL 的结构变更阶段，并验证数据库默认值及已有数据保留。
     */
    @Test
    public void shouldAlterColumnsAndPreserveDefaultsAndData() {
        String ddlSchemaName = tableName + "_alter";
        String quotedSchema = schema.getDialect().quote(ddlSchemaName, false);
        String ddlTableName = "test_ddl_alter";
        System.out.println("[结构变更] 模式=" + mode() + "，临时 Schema=" + ddlSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            RDBSchemaMetadata ddlSchema = createSchemaWithExecutor(ddlSchemaName);
            DatabaseOperator ddlOperator = DefaultDatabaseOperator.of(ddlSchema.getDatabase());
            ddlOperator.ddl().createOrAlter(ddlTableName)
                .comment("结构变更测试")
                .addColumn("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn("name").varchar(64).notNull().comment("名称").commit()
                .addColumn("comment").varchar(32).defaultValueNative("'1'").commit()
                .index().name("index_alter").column("name").commit()
                .commit().reactive().block(TIMEOUT);

            String fullName = ddlSchema.getTable(ddlTableName, false).orElseThrow(AssertionError::new).getFullName();
            String insertSql = "insert into " + fullName + " (" + schema.getDialect().quote("id")
                + ", " + schema.getDialect().quote("name") + ") values (?, ?)";
            String selectSql = "select * from " + fullName + " where " + schema.getDialect().quote("id") + " = ?";
            // 绕过 ORM 默认值填充，确认默认值由数据库执行。
            Assert.assertEquals(Integer.valueOf(1), executor.update(insertSql, "before", "initial").block(TIMEOUT));
            Map<String, Object> before = executor.select(SqlRequests.of(selectSql, "before"),
                lowerCase(ResultWrappers.map())).single().block(TIMEOUT);
            Assert.assertEquals("1", value(before, "comment"));
            System.out.println("[建表默认值回读] " + before);

            // 从实库加载旧结构后扩容字段，并新增普通字段和带默认值字段。
            RDBSchemaMetadata alteringSchema = createSchemaWithExecutor(ddlSchemaName);
            DefaultDatabaseOperator.of(alteringSchema.getDatabase()).ddl().createOrAlter(ddlTableName)
                .addColumn("name").varchar(128).notNull().comment("名称").commit()
                .addColumn("test").varchar(32).comment("test").commit()
                .addColumn("age").number(4).defaultValueNative("0").comment("年龄").commit()
                .commit().reactive().block(TIMEOUT);

            RDBTableMetadata altered = createSchemaWithExecutor(ddlSchemaName)
                .getTableReactive(ddlTableName, true).block(TIMEOUT);
            Assert.assertNotNull(altered);
            Assert.assertEquals(5, altered.getColumns().size());
            Assert.assertEquals(128, altered.getColumnNow("name").getLength());
            Assert.assertTrue(altered.getColumnNow("name").isNotNull());
            Assert.assertEquals(32, altered.getColumnNow("test").getLength());
            Assert.assertEquals(4, altered.getColumnNow("age").getPrecision());
            Assert.assertEquals(0, altered.getColumnNow("age").getScale());
            Assert.assertTrue(altered.getColumnNow("id").isPrimaryKey());
            Assert.assertTrue(altered.getIndex("index_alter").isPresent());
            System.out.println("[字段扩容与新增] name 长度=" + altered.getColumnNow("name").getLength()
                + "，新增 test/age，索引=" + altered.getIndexes());

            Map<String, Object> existing = executor.select(SqlRequests.of(selectSql, "before"),
                lowerCase(ResultWrappers.map())).single().block(TIMEOUT);
            Assert.assertEquals("initial", value(existing, "name"));
            Assert.assertEquals("1", value(existing, "comment"));
            Assert.assertEquals(0, new BigDecimal(String.valueOf(value(existing, "age"))).intValueExact());
            String expandedName = "x".repeat(100);
            Assert.assertEquals(Integer.valueOf(1), executor.update(insertSql, "after", expandedName).block(TIMEOUT));
            Map<String, Object> added = executor.select(SqlRequests.of(selectSql, "after"),
                lowerCase(ResultWrappers.map())).single().block(TIMEOUT);
            Assert.assertEquals(expandedName, value(added, "name"));
            Assert.assertEquals("1", value(added, "comment"));
            Assert.assertEquals(0, new BigDecimal(String.valueOf(value(added, "age"))).intValueExact());
            System.out.println("[新增默认值回读] 原有数据=" + existing + "，新增数据=" + added);

            RDBSchemaMetadata droppingSchema = createSchemaWithExecutor(ddlSchemaName);
            DefaultDatabaseOperator.of(droppingSchema.getDatabase()).ddl().createOrAlter(ddlTableName)
                .dropColumn("test").commit().reactive().block(TIMEOUT);
            RDBTableMetadata dropped = createSchemaWithExecutor(ddlSchemaName)
                .getTableReactive(ddlTableName, true).block(TIMEOUT);
            Assert.assertNotNull(dropped);
            Assert.assertEquals(4, dropped.getColumns().size());
            Assert.assertFalse(dropped.getColumn("test").isPresent());
            Assert.assertEquals(128, dropped.getColumnNow("name").getLength());
            Assert.assertTrue(dropped.getColumnNow("id").isPrimaryKey());
            Assert.assertTrue(dropped.getIndex("index_alter").isPresent());
            Assert.assertEquals("initial", value(executor.select(SqlRequests.of(selectSql, "before"),
                lowerCase(ResultWrappers.map())).single().block(TIMEOUT), "name"));
            Assert.assertEquals(expandedName, value(executor.select(SqlRequests.of(selectSql, "after"),
                lowerCase(ResultWrappers.map())).single().block(TIMEOUT), "name"));
            System.out.println("[删除字段回读] test 已删除，剩余字段数=" + dropped.getColumns().size()
                + "，原有和新增数据、主键及索引均保留");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + ddlSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，字段扩容、新增、默认值、删除及数据/索引保留均符合预期");
    }

    private RDBSchemaMetadata createSchemaWithExecutor(String name) {
        RDBSchemaMetadata created = createSchema(name);
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(created.getDialect());
        database.addSchema(created);
        database.setCurrentSchema(created);
        database.addFeature(executor);
        return created;
    }

    /**
     * 对应官方 testPager/testReactivePager，连续校验十页结果及空数值字段。
     */
    @Test
    public void shouldReadTenConsecutivePagesWithoutDuplicatesOrGaps() {
        String pagingSchemaName = tableName + "_pager";
        String quotedSchema = schema.getDialect().quote(pagingSchemaName, false);
        String pagingTableName = "test_reactive_pager";
        System.out.println("[连续多页分页] 模式=" + mode() + "，临时 Schema=" + pagingSchemaName);
        executor.execute(SqlRequests.of("create schema " + quotedSchema)).block(TIMEOUT);
        try {
            RDBSchemaMetadata pagingSchema = createSchemaWithExecutor(pagingSchemaName);
            DatabaseOperator pagingOperator = DefaultDatabaseOperator.of(pagingSchema.getDatabase());
            pagingOperator.ddl().createOrAlter(pagingTableName)
                .addColumn("id").number(32).primaryKey().comment("ID").commit()
                .addColumn("id2").number(32).comment("ID2").commit()
                .commit().reactive().block(TIMEOUT);
            InsertOperator insert = pagingOperator.dml().insert(pagingTableName).columns("id", "id2");
            for (int id = 1; id <= 100; id++) {
                insert.values(String.valueOf(id), null);
            }
            Assert.assertEquals(Integer.valueOf(100), insert.execute().reactive().block(TIMEOUT));

            List<Integer> allIds = new ArrayList<>();
            for (int pageIndex = 0; pageIndex < 10; pageIndex++) {
                // 显式排序使预期序列稳定，不依赖数据库的物理存储顺序。
                List<Map<String, Object>> rows = pagingOperator.dml().query(pagingTableName)
                    .select("id", "id2").orderBy(SortOrder.asc("id"))
                    .paging(pageIndex, 10)
                    .fetch(lowerCase(ResultWrappers.map())).reactive()
                    .collectList().block(TIMEOUT);
                Assert.assertNotNull(rows);
                Assert.assertEquals(10, rows.size());
                List<Integer> actualIds = rows.stream()
                    .map(row -> ((Number) value(row, "id")).intValue())
                    .collect(Collectors.toList());
                List<Integer> expectedIds = IntStream.rangeClosed(pageIndex * 10 + 1, (pageIndex + 1) * 10)
                    .boxed().collect(Collectors.toList());
                Assert.assertEquals("pageIndex=" + pageIndex, expectedIds, actualIds);
                int expectedSum = ((pageIndex * 10 + 1) + (pageIndex + 1) * 10) * 10 / 2;
                Assert.assertEquals(expectedSum, actualIds.stream().mapToInt(Integer::intValue).sum());
                for (Map<String, Object> row : rows) {
                    Assert.assertNull(value(row, "id2"));
                }
                allIds.addAll(actualIds);
                System.out.println("[分页校验通过] 模式=" + mode() + "，页码=" + pageIndex
                    + "，实际 ID=" + actualIds + "，求和=" + expectedSum + "，id2 均为空");
            }
            Assert.assertEquals(IntStream.rangeClosed(1, 100).boxed().collect(Collectors.toList()), allIds);
            Assert.assertEquals(100L, allIds.stream().distinct().count());
            Assert.assertEquals(5050, allIds.stream().mapToInt(Integer::intValue).sum());

            List<Map<String, Object>> beyondLastPage = pagingOperator.dml().query(pagingTableName)
                .select("id").orderBy(SortOrder.asc("id")).paging(10, 10)
                .fetch(lowerCase(ResultWrappers.map())).reactive().collectList().block(TIMEOUT);
            Assert.assertNotNull(beyondLastPage);
            Assert.assertTrue(beyondLastPage.isEmpty());
            System.out.println("[分页边界校验] 页码从 0 开始，末页页码为 9；页码 10 返回空结果");
        } finally {
            executor.execute(SqlRequests.of("drop schema " + quotedSchema + " cascade")).block(TIMEOUT);
            System.out.println("[测试清理] 已删除临时 Schema=" + pagingSchemaName);
        }
        System.out.println("[验证通过] 模式=" + mode()
            + "，连续 10 页共 100 条，逐页序列和求和正确，无重复、无遗漏，超出末页返回空结果");
    }

    /**
     * 验证通过 EasyORM 响应式接口执行增删改查、批量插入和分页，并正确读写空值及时间字段。
     */
    @Test
    public void shouldExecuteCrudPagingBatchNullAndTimeThroughEasyOrm() {
        System.out.println("[DML 测试] 模式=" + mode() + "，表=" + tableName
            + "，验证增删改查、批量插入、分页及空值和时间字段");
        LocalDateTime createdAt = LocalDateTime.of(2026, 7, 27, 10, 30, 15);

        // 插入两条记录，覆盖 null 和 LocalDateTime 的写入。
        Integer inserted = operator.dml()
                                   .insert(tableName)
                                   .columns("id", "name", "position_no", "nullable_text", "created_at")
                                   .values("one", "before", 1, null, createdAt)
                                   .values("two", "second", 2, "not-null", createdAt.plusSeconds(1))
                                   .execute()
                                   .reactive()
                                   .block(TIMEOUT);
        System.out.println("[新增] 预期影响 2 行，实际=" + inserted);
        Assert.assertEquals(Integer.valueOf(2), inserted);

        // 按主键条件更新一条记录，并检查影响行数。
        Integer updated = operator.dml()
                                  .update(tableName)
                                  .set("name", "after")
                                  .where(where -> where.is("id", "one"))
                                  .execute()
                                  .reactive()
                                  .block(TIMEOUT);
        System.out.println("[修改] id=one，name=after，预期影响 1 行，实际=" + updated);
        Assert.assertEquals(Integer.valueOf(1), updated);

        // 回读验证更新结果，同时确认空值和时间值未发生变化。
        Map<String, Object> row = operator.dml()
                                           .query(tableName)
                                           .select("id", "name", "nullable_text", "created_at")
                                           .where(where -> where.is("id", "one"))
                                           .fetch(lowerCase(ResultWrappers.map()))
                                           .reactive()
                                           .single()
                                           .block(TIMEOUT);
        System.out.println("[查询] id=one，预期 name=after、nullable_text=null、created_at=" + createdAt
            + "，实际=" + row);
        Assert.assertEquals("one", value(row, "id"));
        Assert.assertEquals("after", value(row, "name"));
        Assert.assertNull(value(row, "nullable_text"));
        Assert.assertEquals(createdAt, localDateTime(value(row, "created_at")));

        // 批量追加十条记录，为后续分页查询准备有序数据。
        InsertOperator batchInsert = operator.dml()
                                             .insert(tableName)
                                             .columns("id", "name", "position_no", "created_at");
        for (int i = 3; i <= 12; i++) {
            batchInsert.values("batch-" + i, "batch", i, createdAt.plusSeconds(i));
        }
        Integer batchInserted = batchInsert.execute().reactive().block(TIMEOUT);
        System.out.println("[批量插入] 预期影响 10 行，实际=" + batchInserted);
        Assert.assertEquals(Integer.valueOf(10), batchInserted);

        // 页码从 0 开始：每页三条，第二页应从 position_no=4 开始。
        List<Map<String, Object>> page = operator.dml()
                                                 .query(tableName)
                                                 .select("id", "position_no")
                                                 .orderBy(SortOrder.asc("position_no"))
                                                 .paging(1, 3)
                                                 .fetch(lowerCase(ResultWrappers.map()))
                                                 .reactive()
                                                 .collectList()
                                                 .block(TIMEOUT);
        Assert.assertNotNull(page);
        System.out.println("[分页查询] 页码=1，每页=3，预期首条 position_no=4，实际=" + page);
        Assert.assertEquals(3, page.size());
        Assert.assertEquals(4, ((Number) value(page.get(0), "position_no")).intValue());

        // 按主键条件删除指定记录，预期影响一行。
        Integer deleted = operator.dml()
                                  .delete(tableName)
                                  .where(where -> where.is("id", "two"))
                                  .execute()
                                  .reactive()
                                  .block(TIMEOUT);
        System.out.println("[删除] id=two，预期影响 1 行，实际=" + deleted);
        Assert.assertEquals(Integer.valueOf(1), deleted);
    }

    @Test
    public void shouldSupportReactiveRecordRepositoryCrudAndSave() {
        ReactiveRepository<Record, String> repository = operator.dml().createReactiveRepository(tableName);
        Record record = Record.newRecord()
            .putValue("id", "repository")
            .putValue("name", "before")
            .putValue("position_no", 1);

        Assert.assertEquals(Integer.valueOf(1), repository.insert(record).block(TIMEOUT));

        Record loaded = repository.findById("repository").block(TIMEOUT);
        Assert.assertNotNull(loaded);
        Assert.assertEquals("before", loaded.getString("name").orElse(null));

        Assert.assertEquals(Integer.valueOf(1), repository.createUpdate()
            .set("name", "updated")
            .where("id", "repository")
            .execute()
            .block(TIMEOUT));

        Record saved = Record.newRecord()
            .putValue("id", "repository")
            .putValue("name", "saved")
            .putValue("position_no", 2);
        Assert.assertEquals(1, repository.save(saved).block(TIMEOUT).getTotal());
        Assert.assertEquals("saved", repository.findById("repository")
            .block(TIMEOUT)
            .getString("name")
            .orElse(null));

        Assert.assertEquals(Integer.valueOf(1), repository.deleteById("repository").block(TIMEOUT));
        Assert.assertNull(repository.findById("repository").block(TIMEOUT));
    }

    @Test
    public void shouldTranslateDuplicateKeyDuringReactiveInsert() {
        operator.dml()
            .insert(tableName)
            .columns("id", "name", "position_no")
            .values("duplicate", "first", 1)
            .execute()
            .reactive()
            .block(TIMEOUT);

        StepVerifier.create(operator.dml()
                                  .insert(tableName)
                                  .columns("id", "name", "position_no")
                                  .values("duplicate", "second", 2)
                                  .execute()
                                  .reactive())
            .expectError(DuplicateKeyException.class)
            .verify(Duration.ofSeconds(30));
    }

    @Test
    public void shouldSupportGroupingNativeUpdateNullPredicateAndForUpdate() {
        operator.dml()
            .insert(tableName)
            .columns("id", "name", "position_no", "nullable_text")
            .values("group-1", "same", 1, null)
            .values("group-2", "same", 2, "value")
            .values("group-3", "other", 3, null)
            .execute()
            .reactive()
            .block(TIMEOUT);

        List<Map<String, Object>> grouped = operator.dml()
            .query(tableName)
            .select(Selects.count1().as("total"), () -> SelectColumn.of("name"))
            .groupBy(SelectColumn.of("name"))
            .fetch(lowerCase(ResultWrappers.map()))
            .reactive()
            .collectList()
            .block(TIMEOUT);
        Assert.assertEquals(2, grouped.size());

        Map<String, Object> nullCountRow = operator.dml()
            .query(tableName)
            .where(terms -> terms.isNull("nullable_text").or().is("name", "missing"))
            .select(Selects.count1().as("total"))
            .fetch(lowerCase(ResultWrappers.map()))
            .reactive()
            .single()
            .block(TIMEOUT);
        Assert.assertEquals(2, ((Number) value(nullCountRow, "total")).intValue());

        Assert.assertEquals(Integer.valueOf(1), operator.dml()
            .update(tableName)
            .set("position_no", NativeSql.of("position_no + 1"))
            .where(terms -> terms.is("id", "group-1"))
            .execute()
            .reactive()
            .block(TIMEOUT));

        Map<String, Object> updated = operator.dml()
            .query(tableName)
            .select("position_no")
            .where(terms -> terms.is("id", "group-1"))
            .forUpdate()
            .fetch(lowerCase(ResultWrappers.map()))
            .reactive()
            .single()
            .block(TIMEOUT);
        Assert.assertEquals(2, ((Number) value(updated, "position_no")).intValue());
    }

    /**
     * 对应原生索引解析测试，读取主键、普通、唯一和复合索引并校验索引属性。
     */
    @Test
    public void shouldReadUniqueAndCompositeIndexes() {
        String uniqueIndexName = indexName + "_unique";
        String compositeIndexName = indexName + "_composite";
        String uniqueSql = "create unique index " + uniqueIndexName + " on " + table.getFullName()
            + " (" + schema.getDialect().quote("position_no") + " asc)";
        String compositeSql = "create index " + compositeIndexName + " on " + table.getFullName()
            + " (" + schema.getDialect().quote("name") + " asc, "
            + schema.getDialect().quote("nullable_text") + " desc)";
        System.out.println("[索引读取] 模式=" + mode() + "，建索引 SQL=" + uniqueSql);
        executor.execute(SqlRequests.of(uniqueSql)).block(TIMEOUT);
        System.out.println("[索引读取] 模式=" + mode() + "，建索引 SQL=" + compositeSql);
        executor.execute(SqlRequests.of(compositeSql)).block(TIMEOUT);

        // 直接调用当前兼容模式注册的解析器，从实库读取而非使用建表时的内存索引。
        IndexMetadataParser parser = schema.findFeatureNow(IndexMetadataParser.ID);
        List<RDBIndexMetadata> indexes = parser.parseTableIndexReactive(tableName)
            .collectList()
            .block(TIMEOUT);
        Assert.assertNotNull(indexes);
        for (RDBIndexMetadata index : indexes) {
            System.out.println("[索引实际属性] 模式=" + mode() + "，名称=" + index.getName()
                + "，所属表=" + index.getTableName() + "，唯一=" + index.isUnique()
                + "，主键=" + index.isPrimaryKey() + "，结构=" + index);
            for (RDBIndexMetadata.IndexColumn column : index.getColumns()) {
                System.out.println("[索引字段] 索引=" + index.getName() + "，字段=" + column.getColumn()
                    + "，位置=" + column.getSortIndex() + "，排序=" + column.getSort());
            }
            Assert.assertEquals(tableName, index.getTableName());
        }
        // 初始化已建立主键和普通索引，本次新增唯一索引和复合索引，总计四个。
        Assert.assertEquals(4, indexes.size());

        RDBIndexMetadata ordinary = indexes.stream()
            .filter(index -> indexName.equals(index.getName()))
            .findFirst().orElseThrow(() -> new AssertionError("普通索引未读取到：" + indexName));
        Assert.assertFalse(ordinary.isUnique());
        Assert.assertFalse(ordinary.isPrimaryKey());
        Assert.assertEquals(1, ordinary.getColumns().size());
        Assert.assertEquals("name", ordinary.getColumns().get(0).getColumn());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.asc, ordinary.getColumns().get(0).getSort());
        Assert.assertEquals(1, ordinary.getColumns().get(0).getSortIndex());

        RDBIndexMetadata unique = indexes.stream()
            .filter(index -> uniqueIndexName.equals(index.getName()))
            .findFirst().orElseThrow(() -> new AssertionError("唯一索引未读取到：" + uniqueIndexName));
        Assert.assertTrue(unique.isUnique());
        Assert.assertFalse(unique.isPrimaryKey());
        Assert.assertEquals(1, unique.getColumns().size());
        Assert.assertEquals("position_no", unique.getColumns().get(0).getColumn());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.asc, unique.getColumns().get(0).getSort());
        Assert.assertEquals(1, unique.getColumns().get(0).getSortIndex());

        RDBIndexMetadata composite = indexes.stream()
            .filter(index -> compositeIndexName.equals(index.getName()))
            .findFirst().orElseThrow(() -> new AssertionError("复合索引未读取到：" + compositeIndexName));
        Assert.assertFalse(composite.isUnique());
        Assert.assertFalse(composite.isPrimaryKey());
        Assert.assertEquals(2, composite.getColumns().size());
        Assert.assertEquals("name", composite.getColumns().get(0).getColumn());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.asc, composite.getColumns().get(0).getSort());
        Assert.assertEquals(1, composite.getColumns().get(0).getSortIndex());
        Assert.assertEquals("nullable_text", composite.getColumns().get(1).getColumn());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.desc, composite.getColumns().get(1).getSort());
        Assert.assertEquals(2, composite.getColumns().get(1).getSortIndex());

        Assert.assertEquals(1L, indexes.stream().filter(RDBIndexMetadata::isPrimaryKey).count());
        RDBIndexMetadata primary = indexes.stream().filter(RDBIndexMetadata::isPrimaryKey)
            .findFirst().orElseThrow(AssertionError::new);
        Assert.assertTrue(primary.isUnique());
        Assert.assertEquals(1, primary.getColumns().size());
        Assert.assertEquals("id", primary.getColumns().get(0).getColumn());
        Assert.assertEquals(RDBIndexMetadata.IndexSort.asc, primary.getColumns().get(0).getSort());
        Assert.assertEquals(1, primary.getColumns().get(0).getSortIndex());
        System.out.println("[验证通过] 模式=" + mode()
            + "，共读取 4 个索引，唯一性、主键标识、所属表、字段顺序及升降序均符合预期");
    }

    @Test
    public void shouldAllowRepeatedIndexCreationThroughDdl() {
        System.out.println("[索引结构同步] 模式=" + mode() + "，重复声明已有索引=" + indexName);
        operator.ddl()
            .createOrAlter(tableName)
            .index()
            .name(indexName)
            .column("name")
            .commit()
            .commit()
            .reactive()
            .block(TIMEOUT);

        RDBIndexMetadata index = schema
            .findFeatureNow(IndexMetadataParser.ID)
            .parseByNameReactive(indexName)
            .cast(RDBIndexMetadata.class)
            .block(TIMEOUT);
        Assert.assertNotNull(index);
        Assert.assertEquals(indexName, index.getName());
        Assert.assertEquals(tableName, index.getTableName());
        Assert.assertEquals(1, index.getColumns().size());
        Assert.assertEquals("name", index.getColumns().get(0).getColumn());
        System.out.println("[验证结果] 已有索引结构不变，重复同步成功，索引字段=name");
    }

    @Test
    public void shouldRebuildIndexWhenColumnsChange() {
        // Read the existing index from the database instead of using initialization metadata.
        RDBSchemaMetadata reloadedSchema = createSchema(schema.getName());
        RDBDatabaseMetadata reloadedDatabase = new RDBDatabaseMetadata(reloadedSchema.getDialect());
        reloadedDatabase.addSchema(reloadedSchema);
        reloadedDatabase.setCurrentSchema(reloadedSchema);
        reloadedDatabase.addFeature(executor);
        DatabaseOperator reloadedOperator = DefaultDatabaseOperator.of(reloadedDatabase);

        System.out.println("[索引结构变更] 模式=" + mode() + "，索引=" + indexName
            + "，字段由 name 改为 position_no");
        reloadedOperator.ddl()
            .createOrAlter(tableName)
            .index()
            .name(indexName)
            .column("position_no")
            .commit()
            .commit()
            .reactive()
            .block(TIMEOUT);

        RDBIndexMetadata index = reloadedSchema
            .findFeatureNow(IndexMetadataParser.ID)
            .parseByNameReactive(indexName)
            .cast(RDBIndexMetadata.class)
            .block(TIMEOUT);
        Assert.assertNotNull(index);
        Assert.assertEquals(indexName, index.getName());
        Assert.assertEquals(tableName, index.getTableName());
        Assert.assertEquals(1, index.getColumns().size());
        Assert.assertEquals("position_no", index.getColumns().get(0).getColumn());
        System.out.println("[验证结果] 同名索引已按新结构重建，索引字段=position_no");
    }

    @Test
    public void shouldQueryEnumMaskUsingModeSpecificBitAnd() {
        RDBColumnMetadata position = table.getColumn("position_no")
            .orElseThrow(AssertionError::new);
        position.setValueCodec(new EnumValueCodec(Permission[].class, true));
        Assert.assertNotNull(table.getOnColumnAdded());
        table.getOnColumnAdded().accept(position);
        Assert.assertTrue(position.findFeature(EnumInFragmentBuilder.of(schema.getDialect()).getId()).isPresent());

        operator.dml()
            .insert(tableName)
            .columns("id", "name", "position_no")
            .values("mask", "mask", new Permission[]{Permission.READ, Permission.ADMIN})
            .execute()
            .reactive()
            .block(TIMEOUT);

        Map<String, Object> hasAdmin = operator.dml()
            .query(tableName)
            .where(Term.of("position_no", "in$any", Permission.ADMIN))
            .select(Selects.count1().as("total"))
            .fetch(lowerCase(ResultWrappers.map()))
            .reactive()
            .single()
            .block(TIMEOUT);
        Assert.assertEquals(1, ((Number) value(hasAdmin, "total")).intValue());

        Map<String, Object> hasWrite = operator.dml()
            .query(tableName)
            .where(Term.of("position_no", "in$any", Permission.WRITE))
            .select(Selects.count1().as("total"))
            .fetch(lowerCase(ResultWrappers.map()))
            .reactive()
            .single()
            .block(TIMEOUT);
        Assert.assertEquals(0, ((Number) value(hasWrite, "total")).intValue());

        Map<String, Object> excludesWrite = operator.dml()
            .query(tableName)
            .where(Term.of("position_no", "nin$any", Permission.WRITE))
            .select(Selects.count1().as("total"))
            .fetch(lowerCase(ResultWrappers.map()))
            .reactive()
            .single()
            .block(TIMEOUT);
        Assert.assertEquals(1, ((Number) value(excludesWrite, "total")).intValue());
    }

    @Test
    public void shouldRoundTripClobAndJsonClobThroughEasyOrm() {
        Object content = ClobValueCodec.INSTANCE.encode(
            "kingbase-" + mode() + "-clob",
            table.getColumn("content").orElseThrow(AssertionError::new)
        );

        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("mode", mode());
        payload.put("value", 42);
        JsonValueCodec jsonCodec = JsonValueCodec.of(Map.class);
        Object json = jsonCodec.encode(
            payload,
            table.getColumn("json_content").orElseThrow(AssertionError::new)
        );

        Assert.assertTrue(content instanceof ClobValue);
        Assert.assertTrue(json instanceof ClobValue);

        Integer inserted = operator.dml()
                                   .insert(tableName)
                                   .columns("id", "name", "position_no", "content", "json_content")
                                   .values("lob", "lob", 1, content, json)
                                   .execute()
                                   .reactive()
                                   .block(TIMEOUT);
        Assert.assertEquals(Integer.valueOf(1), inserted);

        Map<String, Object> row = executor.select(
                SqlRequests.of("select content, json_content from " + table.getFullName() + " where id = ?", "lob"),
                lowerCase(ResultWrappers.map())
            )
            .single()
            .block(TIMEOUT);

        Assert.assertEquals("kingbase-" + mode() + "-clob", ClobValueCodec.INSTANCE.decode(value(row, "content")));
        Object decodedJson = jsonCodec.decode(value(row, "json_content"));
        Assert.assertTrue(decodedJson instanceof Map);
        Assert.assertEquals(mode(), String.valueOf(((Map<?, ?>) decodedJson).get("mode")));
        Assert.assertEquals(42, ((Number) ((Map<?, ?>) decodedJson).get("value")).intValue());
    }

    @Test
    public void shouldRollbackToSavepointAndCommitTransaction() {
        Connection connection = Mono.from(connectionFactory.create()).block(TIMEOUT);
        Assert.assertNotNull(connection);

        try {
            Mono.from(connection.beginTransaction()).block(TIMEOUT);
            insertTransactionRow(connection, "savepoint-before");
            Mono.from(connection.createSavepoint("after_first")).block(TIMEOUT);
            insertTransactionRow(connection, "savepoint-after");
            Mono.from(connection.rollbackTransactionToSavepoint("after_first")).block(TIMEOUT);
            Mono.from(connection.commitTransaction()).block(TIMEOUT);

            Long count = Flux.from(connection.createStatement(
                    "select count(1) as total from " + table.getFullName()
                        + " where id like $1"
                )
                .bind("$1", "savepoint-%")
                .execute())
                .flatMap(result -> result.map((row, metadata) -> row.get("total", Number.class).longValue()))
                .single()
                .block(TIMEOUT);
            Assert.assertEquals(Long.valueOf(1), count);
        } finally {
            Mono.from(connection.close()).block(TIMEOUT);
        }
    }

    @Test
    public void shouldRollbackTransaction() {
        Connection connection = Mono.from(connectionFactory.create()).block(TIMEOUT);
        Assert.assertNotNull(connection);

        try {
            Mono.from(connection.beginTransaction()).block(TIMEOUT);
            Flux.from(connection.createStatement(
                    "insert into " + table.getFullName() + " (id, name, position_no) values ($1, $2, $3)"
                )
                .bind("$1", "rollback")
                .bind("$2", "rollback")
                .bind("$3", 1)
                .execute())
                .flatMap(result -> result.getRowsUpdated())
                .single()
                .block(TIMEOUT);
            Mono.from(connection.rollbackTransaction()).block(TIMEOUT);

            Long count = Flux.from(connection.createStatement(
                    "select count(1) as total from " + table.getFullName() + " where id = $1"
                )
                .bind("$1", "rollback")
                .execute())
                .flatMap(result -> result.map((row, metadata) -> row.get("total", Number.class).longValue()))
                .single()
                .block(TIMEOUT);
            Assert.assertEquals(Long.valueOf(0), count);
        } finally {
            Mono.from(connection.close()).block(TIMEOUT);
        }
    }

    private void insertTransactionRow(Connection connection, String id) {
        Flux.from(connection.createStatement(
                "insert into " + table.getFullName() + " (id, name, position_no) values ($1, $2, $3)"
            )
            .bind("$1", id)
            .bind("$2", id)
            .bind("$3", 1)
            .execute())
            .flatMap(result -> result.getRowsUpdated())
            .single()
            .block(TIMEOUT);
    }

    private String configuration(String name, String defaultValue) {
        String value = System.getProperty(prefix() + "." + name);
        if (value == null || value.isEmpty()) {
            value = System.getenv(environmentName(name));
        }
        return value == null || value.isEmpty() ? defaultValue : value;
    }

    private String prefix() {
        return "kingbase." + mode() + ".r2dbc";
    }

    private String environmentName(String name) {
        return ("KINGBASE_" + mode() + "_R2DBC_" + name).toUpperCase(Locale.ROOT).replace('.', '_');
    }

    private static Object value(Map<String, Object> row, String name) {
        Object value = row.get(name);
        if (value != null || row.containsKey(name)) {
            return value;
        }
        for (Map.Entry<String, Object> entry : row.entrySet()) {
            if (entry.getKey().equalsIgnoreCase(name)) {
                return entry.getValue();
            }
        }
        return null;
    }

    private String queryDatabaseMode(Connection connection) {
        return Flux.from(connection.createStatement("show database_mode").execute())
            .flatMap(result -> result.map((row, metadata) -> row.get(0, String.class)))
            .single()
            .map(value -> value.toLowerCase(Locale.ROOT))
            .block(TIMEOUT);
    }

    private enum Permission {
        READ,
        WRITE,
        ADMIN
    }

    private static LocalDateTime localDateTime(Object value) {
        Assert.assertNotNull("The R2DBC driver returned a null timestamp", value);
        if (value instanceof LocalDateTime) {
            return (LocalDateTime) value;
        }
        if (value instanceof OffsetDateTime) {
            return ((OffsetDateTime) value).toLocalDateTime();
        }
        if (value instanceof Timestamp) {
            return ((Timestamp) value).toLocalDateTime();
        }
        if (value instanceof Date) {
            return new Timestamp(((Date) value).getTime()).toLocalDateTime();
        }
        throw new AssertionError("Unsupported R2DBC timestamp type: " + value.getClass().getName());
    }
}
