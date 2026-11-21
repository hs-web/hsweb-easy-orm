package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.codec.EnumValueCodec;
import org.hswebframework.ezorm.rdb.codec.DateTimeCodec;
import org.hswebframework.ezorm.rdb.executor.BatchSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.NativeSqlDefaultValue;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import org.hswebframework.ezorm.rdb.metadata.parser.TableMetadataParser;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexParameter;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.DropIndexSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerAlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerCreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerJsonTermFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerSchemaMetadata;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.sql.Timestamp;
import java.util.Date;

public class KingbaseSqlServerSchemaMetadataTest {

    private final KingbaseSqlServerSchemaMetadata schema = new KingbaseSqlServerSchemaMetadata("dbo");
    private final RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());

    @Before
    public void initDatabase() {
        database.addSchema(schema);
        database.setCurrentSchema(schema);
    }

    @Test
    public void shouldInheritNativeSqlServerAndReplaceOnlyKingbaseFeatures() {
        Assert.assertTrue(schema instanceof SqlServerSchemaMetadata);
        Assert.assertTrue(schema.getDialect() instanceof KingbaseSqlServerDialect);
        Assert.assertTrue(schema.findFeatureNow(CreateTableSqlBuilder.ID) instanceof SqlServerCreateTableSqlBuilder);
        Assert.assertTrue(schema.findFeatureNow(AlterTableSqlBuilder.ID) instanceof SqlServerAlterTableSqlBuilder);
        Assert.assertTrue(schema.findFeatureNow(TableMetadataParser.id) instanceof KingbaseSqlServerTableMetadataParser);
        Assert.assertTrue(schema.findFeatureNow(IndexMetadataParser.ID) instanceof KingbaseSqlServerIndexMetadataParser);
    }

    @Test
    public void shouldRenderSqlServerCreateTableAndKingbaseComments() {
        RDBTableMetadata table = newTable();
        table.setComment("账户表");
        table.getColumn("name").get().setComment("账户名称");
        table.getColumn("id").get().setDefaultValue(NativeSqlDefaultValue.of("newid()"));

        BatchSqlRequest request = (BatchSqlRequest) schema.findFeatureNow(CreateTableSqlBuilder.ID).build(table);
        Assert.assertEquals("create table [dbo].account ( [id] nvarchar(32) not null primary key default newid() , [name] nvarchar(64) )",
            request.getSql());
        Assert.assertEquals(2, request.getBatch().size());
        Assert.assertEquals("comment on column [dbo].account.[name] is '账户名称'", request.getBatch().get(0).getSql());
        Assert.assertEquals("comment on table [dbo].account is '账户表'", request.getBatch().get(1).getSql());
    }

    @Test
    public void shouldRenderNativeAlterColumn() {
        RDBTableMetadata oldTable = newTable();
        RDBTableMetadata newTable = newTable();
        newTable.getColumn("name").get().setLength(128);
        newTable.getColumn("name").get().setNotNull(true);
        AlterRequest alter = new AlterRequest();
        alter.setOldTable(oldTable);
        alter.setNewTable(newTable);
        BatchSqlRequest request = (BatchSqlRequest) schema.findFeatureNow(AlterTableSqlBuilder.ID).build(alter);
        Assert.assertEquals("alter table [dbo].account alter column [name] nvarchar(128) not null", request.getSql());
        Assert.assertTrue(request.getBatch().isEmpty());
    }

    @Test
    public void shouldKeepNativeIndexCreateAndDropSyntax() {
        RDBTableMetadata table = newTable();
        RDBIndexMetadata index = new RDBIndexMetadata("idx_account_name");
        index.setUnique(true);
        RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
        column.setColumn("name");
        column.setSort(RDBIndexMetadata.IndexSort.desc);
        index.getColumns().add(column);
        CreateIndexParameter parameter = CreateIndexParameter.of(table, index);
        String create = schema.findFeatureNow(CreateIndexSqlBuilder.ID).build(parameter).getSql();
        Assert.assertTrue(create, create.contains("unique"));
        Assert.assertTrue(create, create.contains("idx_account_name on [dbo].account"));
        Assert.assertTrue(create, create.contains("[name] desc"));
        Assert.assertEquals("drop index idx_account_name on [dbo].account",
            schema.findFeatureNow(DropIndexSqlBuilder.ID).build(parameter).getSql());
    }

    @Test
    public void shouldRetainQueryPaginationAndParameterOrder() {
        schema.addTable(newTable());
        SqlRequest request = DefaultDatabaseOperator.of(database).dml().query("account")
            .select("id", "name").where(Term.of("name", "eq", "张三")).orderBy(SortOrder.asc("id")).paging(2, 10).getSql();
        Assert.assertTrue(request.getSql(), request.getSql().contains("offset ? rows fetch next ? rows only"));
        Assert.assertFalse(request.getSql().contains("order by (select null)"));
        Assert.assertArrayEquals(new Object[]{"张三", 20, 10}, request.getParameters());
    }

    @Test
    public void shouldUseSqlServerUpdateLockHint() {
        schema.addTable(newTable());
        QueryOperator query = DefaultDatabaseOperator.of(database).dml().query("account");
        query.forUpdate();
        Assert.assertTrue(query.getSql().getSql(), query.getSql().getSql().contains("with(updlock)"));
    }

    @Test
    public void shouldRetainEnumAndJsonColumnFeatures() {
        RDBTableMetadata table = newTable();
        RDBColumnMetadata state = table.newColumn();
        state.setName("state");
        state.setJdbcType(JDBCType.BIGINT, Long.class);
        state.setValueCodec(new EnumValueCodec(State.class, true));
        table.addColumn(state);
        Assert.assertSame(SqlServerEnumInFragmentBuilder.in,
            state.findFeatureNow(SqlServerEnumInFragmentBuilder.in.getId()));
        RDBColumnMetadata json = table.newColumn();
        json.setName("document");
        json.setType(schema.getDialect().convertDataType("json"));
        table.addColumn(json);
        Assert.assertSame(KingbaseSqlServerJsonTermFragmentBuilder.value,
            json.findFeatureNow(SqlServerJsonTermFragmentBuilder.value.getId()));
    }

    @Test
    public void shouldDecodeSevenDigitDatetime2StringsWithTheNativeCodec() {
        RDBTableMetadata table = schema.newTable("time_probe");
        RDBColumnMetadata column = table.newColumn();
        column.setName("created_at");
        column.setJdbcType(JDBCType.TIMESTAMP, Date.class);
        column.setValueCodec(new DateTimeCodec("yyyy-MM-dd HH:mm:ss", Date.class));
        table.addColumn(column);
        Date decoded = (Date) column.decode("2026-11-10 18:17:55.3940000");
        Assert.assertEquals(Timestamp.valueOf("2026-11-10 18:17:55.394").getTime(), decoded.getTime());
        Assert.assertNull(column.decode(null));
        Date value = new Date(123456789L);
        Assert.assertSame(value, column.encode(value));
        Assert.assertEquals(123456789L, ((Date) column.decode("123456789")).getTime());
    }

    @Test
    public void shouldEscapeCommentQuotesAndHonorTheNativeCommentToggle() {
        RDBTableMetadata table = newTable();
        table.setComment("owner's table");
        table.getColumn("name").get().setComment("owner's 中文名称");
        KingbaseSqlServerCreateTableSqlBuilder builder = new KingbaseSqlServerCreateTableSqlBuilder();
        BatchSqlRequest request = (BatchSqlRequest) builder.build(table);
        Assert.assertEquals("comment on column [dbo].account.[name] is 'owner''s 中文名称'", request.getBatch().get(0).getSql());
        Assert.assertEquals("comment on table [dbo].account is 'owner''s table'", request.getBatch().get(1).getSql());
        Assert.assertEquals(0, request.getBatch().get(0).getParameters().length);
        builder.setCreateComment(false);
        Assert.assertTrue(((BatchSqlRequest) builder.build(table)).getBatch().isEmpty());
    }

    private RDBTableMetadata newTable() {
        RDBTableMetadata table = schema.newTable("account");
        RDBColumnMetadata id = table.newColumn();
        id.setName("id");
        id.setJdbcType(JDBCType.VARCHAR, String.class);
        id.setLength(32);
        id.setPrimaryKey(true);
        table.addColumn(id);
        RDBColumnMetadata name = table.newColumn();
        name.setName("name");
        name.setJdbcType(JDBCType.VARCHAR, String.class);
        name.setLength(64);
        table.addColumn(name);
        return table;
    }

    private enum State { enabled, disabled }
}
