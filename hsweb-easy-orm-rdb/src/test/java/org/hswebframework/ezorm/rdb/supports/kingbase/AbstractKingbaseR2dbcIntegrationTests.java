package org.hswebframework.ezorm.rdb.supports.kingbase;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactory;
import io.r2dbc.spi.ConnectionFactoryOptions;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.codec.ClobValue;
import org.hswebframework.ezorm.rdb.codec.ClobValueCodec;
import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.codec.JsonValueCodec;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.EnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.operator.dml.query.Selects;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
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

import java.sql.Timestamp;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

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
