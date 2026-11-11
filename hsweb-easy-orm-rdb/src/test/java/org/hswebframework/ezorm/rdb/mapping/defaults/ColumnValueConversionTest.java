package org.hswebframework.ezorm.rdb.mapping.defaults;

import io.r2dbc.spi.Connection;
import io.r2dbc.spi.ConnectionFactories;
import io.r2dbc.spi.ConnectionFactoryOptions;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.DictionaryCodec;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.core.meta.ColumnMetadata;
import org.hswebframework.ezorm.rdb.R2dbcConnectionProvider;
import org.hswebframework.ezorm.rdb.TestReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.DefaultColumnWrapperContext;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.MapResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.RecordResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.annotation.EnumCodec;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.wrapper.NestedEntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.ValueConverterResultWrapper;
import org.hswebframework.ezorm.rdb.supports.h2.H2ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Mono;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.Table;
import java.sql.JDBCType;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.Assert.*;

/** Uses real JDBC/R2DBC queries so repository and collector wrappers participate in decoding. */
public class ColumnValueConversionTest {
    public enum State { pending, completed }

    @Getter
    @Setter
    @Table(name = "decode_once")
    public static class Row {
        @Id @Column private String id;
        @Column @ColumnType(jdbcType = JDBCType.BIGINT, javaType = State[].class) @EnumCodec(toMask = true)
        private State[] flags;
        @Column private String label;
    }

    @Getter
    @Setter
    @Table(name = "nested_decode_once")
    public static class Parent extends Row {
        @Column(name = "child_id") private String childId;
        @JoinColumn(name = "child_id") private Row child;
    }

    @Getter
    @Setter
    @Table(name = "decode_view")
    public static class View {
        @Column private String label;
    }

    private DatabaseOperator database;
    private RDBTableMetadata table;
    private EntityColumnMapping mapping;
    private Connection reactive;
    private H2ConnectionProvider jdbc;
    private final AtomicInteger decoded = new AtomicInteger();

    @Before
    public void prepare() throws Exception {
        jdbc = new H2ConnectionProvider();
        String name = jdbc.getConnection().getMetaData().getURL().substring("jdbc:h2:mem:".length());
        ConnectionFactoryOptions options = ConnectionFactoryOptions.parse("r2dbc:h2:mem:///" + name).mutate()
            .option(ConnectionFactoryOptions.USER, "sa")
            .option(ConnectionFactoryOptions.PASSWORD, "")
            .build();
        reactive = Mono.from(ConnectionFactories.get(options).create()).block();
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        metadata.addSchema(schema);
        metadata.setCurrentSchema(schema);
        metadata.addFeature(new TestSyncSqlExecutor(jdbc));
        metadata.addFeature(new TestReactiveSqlExecutor(new R2dbcConnectionProvider() {
            @Override public Mono<Connection> getConnection() { return Mono.just(reactive); }
            @Override public void releaseConnection(Connection connection) { }
        }));
        database = DefaultDatabaseOperator.of(metadata);
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(metadata);
        table = parser.parseTableMetadata(Row.class).orElseThrow(AssertionError::new);
        database.ddl().createOrAlter(table).commit().sync();
        mapping = table.getFeatureNow(MappingFeatureType.columnPropertyMapping.createFeatureId(Row.class));
        mapping.reload();
        table.getColumnNow("label").setValueCodec(new ValueCodec<Object, Object>() {
            @Override public Object encode(Object value) { return value; }
            @Override public Object decode(Object value) { decoded.incrementAndGet(); return value + "!"; }
        });
        database.sql().sync().execute(SqlRequests.of("insert into decode_once(id, flags, label) values ('one', 3, 'raw')"));
    }

    @After
    public void close() throws Exception {
        if (reactive != null) { Mono.from(reactive.close()).block(); }
        if (jdbc != null) { jdbc.getConnection().close(); }
    }

    private EntityResultWrapper<Row> entity() { return new EntityResultWrapper<>(Row::new, mapping); }

    @Test
    public void syncRepositoryKeepsEnumArrays() {
        Row result = new DefaultSyncRepository<>(database, table, Row.class, entity()).findById("one").orElseThrow(AssertionError::new);
        assertArrayEquals(new State[] {State.pending, State.completed}, result.getFlags());
    }

    @Test
    public void reactiveRepositoryDecodesCustomCodecOnce() {
        Row result = new DefaultReactiveRepository<>(database, table, Row.class, entity()).findById("one").block();
        assertEquals("raw!", result.getLabel());
        assertEquals(1, decoded.get());
        assertArrayEquals(new State[] {State.pending, State.completed}, result.getFlags());
    }

    @Test
    public void mapAndSingleColumnKeepTheirDecodingContracts() {
        Map<String, Object> row = database.dml().query(table).fetch(ResultWrappers.singleMap()).sync();
        assertEquals("raw!", row.get("label"));
        assertEquals(1, decoded.get());
        assertArrayEquals(new State[] {State.pending, State.completed}, (State[]) row.get("flags"));
        decoded.set(0);
        String wrapped = database.dml().query(table).select("label")
                             .fetch(ResultWrappers.single(ResultWrappers.column("label", String::valueOf))).sync();
        assertEquals("raw!", wrapped);
        assertEquals(1, decoded.get());
        decoded.set(0);
        String raw = database.dml().query(table).select("label")
                             .fetch(ResultWrappers.column("label", String::valueOf)).reactive().single().block();
        assertEquals("raw", raw);
        assertEquals(0, decoded.get());
    }

    @Test
    public void collectionAndTransformWrappersPreserveEntityDecodingOwnership() {
        List<Row> rows = database.dml().query(table).fetch(ResultWrappers.list(entity())).sync();
        assertEquals("raw!", rows.get(0).getLabel());
        assertEquals(1, decoded.get());
        decoded.set(0);
        String value = database.dml().query(table).fetch(ResultWrappers.convert(ResultWrappers.single(entity()), Row::getLabel)).sync();
        assertEquals("raw!", value);
        assertEquals(1, decoded.get());
    }

    @Test
    public void executorAndUnmappedEntityKeepTheirDecodingContracts() {
        Row direct = database.sql().sync().select(database.dml().query(table).getSql(), ResultWrappers.single(entity()));
        assertEquals("raw!", direct.getLabel());
        assertEquals(1, decoded.get());
        decoded.set(0);
        Row unmapped = database.dml().query(table)
                               .fetch(ResultWrappers.single(new EntityResultWrapper<>(Row::new))).sync();
        assertEquals("raw!", unmapped.getLabel());
        assertEquals(1, decoded.get());
    }

    @Test
    public void recordAndConsumerWrappersDecodeOnce() {
        Record record = database.dml().query(table).fetch(ResultWrappers.single(RecordResultWrapper.of(mapping))).sync();
        assertEquals("raw!", record.getString("label").orElseThrow(AssertionError::new));
        assertEquals(1, decoded.get());
        decoded.set(0);
        List<Row> rows = new ArrayList<>();
        Integer count = database.dml().query(table)
                                .fetch(ResultWrappers.consumer(ResultWrappers.lowerCase(entity()), rows::add)).sync();
        assertEquals(Integer.valueOf(1), count);
        assertEquals("raw!", rows.get(0).getLabel());
        assertEquals(1, decoded.get());
    }

    @Test
    public void explicitValueConverterIsNotAppliedTwice() {
        Map<String, Object> row = database.dml().query(table)
            .fetch(ValueConverterResultWrapper.of(ResultWrappers.singleMap(), table)).sync();
        assertEquals("raw!", row.get("label"));
        assertEquals(1, decoded.get());
    }

    @Test
    public void queryColumnEventsKeepDecodedValues() {
        AtomicReference<Object> value = new AtomicReference<>();
        table.addFeature((EventListener) (type, context) -> {
            if (type == MappingEventTypes.select_wrapper_column) {
                ColumnWrapperContext<?> column = (ColumnWrapperContext<?>) context.get(MappingContextKeys.context).orElseThrow(AssertionError::new);
                if ("label".equals(column.getColumnLabel())) {
                    assertSame(table.getColumnNow("label"), column.getDecodedColumn());
                    value.set(column.getResult());
                }
            }
        });
        new DefaultSyncRepository<>(database, table, Row.class, entity()).findById("one").orElseThrow(AssertionError::new);
        assertEquals("raw!", value.get());
        assertEquals(1, decoded.get());
    }

    @Test
    public void unchangedDecodedValuesAreStillMarkedConverted() {
        table.getColumnNow("label").setValueCodec(new ValueCodec<Object, Object>() {
            @Override public Object encode(Object value) { return value; }
            @Override public Object decode(Object value) { decoded.incrementAndGet(); return value; }
        });
        Row result = database.dml().query(table).fetch(ResultWrappers.single(ResultWrappers.lowerCase(entity()))).sync();
        assertEquals("raw", result.getLabel());
        assertEquals(1, decoded.get());
    }

    private RDBTableMetadata viewTable() {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(table.getSchema().getDatabase());
        RDBTableMetadata view = parser.parseTableMetadata(View.class).orElseThrow(AssertionError::new);
        EntityColumnMapping viewMapping = view.getFeatureNow(MappingFeatureType.columnPropertyMapping.createFeatureId(View.class));
        viewMapping.reload();
        return view;
    }

    private void assertProjection(RDBTableMetadata view, String expected) {
        EntityColumnMapping viewMapping = view.getFeatureNow(MappingFeatureType.columnPropertyMapping.createFeatureId(View.class));
        View entity = database.dml().query(table).select("label")
            .fetch(ResultWrappers.single(new EntityResultWrapper<>(View::new, viewMapping))).sync();
        assertEquals(expected, entity.getLabel());
        Record record = database.dml().query(table).select("label")
            .fetch(ResultWrappers.single(RecordResultWrapper.of(viewMapping))).sync();
        assertEquals(expected, record.getString("label").orElseThrow(AssertionError::new));
    }

    @Test
    public void plainSourceStillAppliesTargetEntityAndRecordCodec() {
        RDBTableMetadata view = viewTable();
        view.getColumnNow("label").setValueCodec(table.getColumnNow("label").getValueCodec());
        table.getColumnNow("label").setValueCodec(null);
        assertProjection(view, "raw!");
        assertEquals(2, decoded.get());
    }

    @Test
    public void sourceAndDifferentTargetCodecsBothRun() {
        RDBTableMetadata view = viewTable();
        view.getColumnNow("label").setValueCodec(new ValueCodec<Object, Object>() {
            @Override public Object encode(Object value) { return value; }
            @Override public Object decode(Object value) { return value + "?"; }
        });
        assertProjection(view, "raw!?");
        assertEquals(2, decoded.get());
    }

    @Test
    public void sharedCodecStillUsesEachColumnMetadata() {
        RDBTableMetadata view = viewTable();
        ValueCodec<Object, Object> codec = new ValueCodec<Object, Object>() {
            @Override public Object encode(Object value) { return value; }
            @Override public Object decode(Object value) { throw new AssertionError("column metadata required"); }
            @Override public Object decode(Object value, ColumnMetadata column) { return value + column.getComment(); }
        };
        table.getColumnNow("label").setValueCodec(codec);
        table.getColumnNow("label").setComment("-source");
        view.getColumnNow("label").setValueCodec(codec);
        view.getColumnNow("label").setComment("-target");
        assertProjection(view, "raw-source-target");
    }

    @Test
    public void dictionaryOnlyColumnIsDecodedOnce() {
        table.getColumnNow("label").setValueCodec(null);
        table.getColumnNow("label").setDictionaryCodec(new DictionaryCodec() {
            @Override public Collection<Object> getItems() { return Collections.emptyList(); }
            @Override public String getFieldName() { return "labelText"; }
            @Override public Object encode(Object value) { return value; }
            @Override public Object decode(Object value) { decoded.incrementAndGet(); return value + "!"; }
        });
        Row result = database.dml().query(table).fetch(ResultWrappers.single(entity())).sync();
        assertEquals("raw!", result.getLabel());
        assertEquals(1, decoded.get());
    }

    @Test
    public void plainColumnReusesOriginalContext() {
        table.getColumnNow("id").setValueCodec(null);
        AtomicReference<ColumnWrapperContext<?>> received = new AtomicReference<>();
        MapResultWrapper capture = new MapResultWrapper() {
            @Override public void wrapColumn(ColumnWrapperContext<Map<String, Object>> context) {
                received.set(context);
                super.wrapColumn(context);
            }
        };
        DefaultColumnWrapperContext<Map<String, Object>> context =
            new DefaultColumnWrapperContext<>(0, "id", "one", capture.newRowInstance());
        ValueConverterResultWrapper.of(capture, table).wrapColumn(context);
        assertSame(context, received.get());
        assertNull(received.get().getDecodedColumn());
    }

    @Test
    public void copiedContextPreservesDecodedColumn() {
        EntityResultWrapper<Row> target = entity();
        EntityResultWrapper<Row> copying = new EntityResultWrapper<Row>(Row::new) {
            @Override public void wrapColumn(ColumnWrapperContext<Row> context) {
                target.wrapColumn(DefaultColumnWrapperContext.copyOf(context));
            }
        };
        Row result = database.dml().query(table).fetch(ResultWrappers.single(copying)).sync();
        assertEquals("raw!", result.getLabel());
        assertEquals(1, decoded.get());
    }

    @Test
    public void lowerCaseWrapperPropagatesReplacementRowAndDecodedColumn() {
        EntityResultWrapper<Row> replacing = new EntityResultWrapper<Row>(Row::new, mapping) {
            @Override public void wrapColumn(ColumnWrapperContext<Row> context) {
                assertEquals("label", context.getColumnLabel());
                assertSame(table.getColumnNow("label"), context.getDecodedColumn());
                context.setRowInstance(new Row());
                super.wrapColumn(context);
            }
        };
        Row original = new Row();
        DefaultColumnWrapperContext<Row> context = new DefaultColumnWrapperContext<>(
            0, "LABEL", "raw!", original, table.getColumnNow("label"));
        ResultWrappers.lowerCase(replacing).wrapColumn(context);
        assertNotSame(original, context.getRowInstance());
        assertEquals("raw!", context.getRowInstance().getLabel());
        assertEquals("LABEL", context.getColumnLabel());
        assertEquals(0, decoded.get());
    }

    @Test
    public void nestedEntityDecodesRootAndAssociatedColumnsOnce() {
        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(table.getSchema().getDatabase());
        RDBTableMetadata parent = parser.parseTableMetadata(Parent.class).orElseThrow(AssertionError::new);
        database.ddl().createOrAlter(parent).commit().sync();
        parent.getColumnNow("label").setValueCodec(table.getColumnNow("label").getValueCodec());
        EntityColumnMapping parentMapping = parent.getFeatureNow(MappingFeatureType.columnPropertyMapping.createFeatureId(Parent.class));
        parentMapping.reload();
        database.sql().sync().execute(SqlRequests.of("insert into nested_decode_once(id, flags, label, child_id) values ('parent', 3, 'root', 'one')"));
        Parent result = database.dml().query(parent).select("id", "label", "flags", "child.label", "child.flags")
                                .fetch(ResultWrappers.single(new NestedEntityResultWrapper<Parent>(parentMapping))).sync();
        assertEquals("root!", result.getLabel());
        assertEquals("raw!", result.getChild().getLabel());
        assertArrayEquals(new State[] {State.pending, State.completed}, result.getFlags());
        assertArrayEquals(new State[] {State.pending, State.completed}, result.getChild().getFlags());
        assertEquals(2, decoded.get());
    }
}
