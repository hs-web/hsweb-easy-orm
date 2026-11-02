package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.events.ContextKey;
import org.hswebframework.ezorm.rdb.events.ContextKeys;
import org.hswebframework.ezorm.rdb.events.EventContext;
import org.hswebframework.ezorm.rdb.events.EventListener;
import org.hswebframework.ezorm.rdb.events.EventType;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.SyncDelete;
import org.hswebframework.ezorm.rdb.mapping.SyncUpdate;
import org.hswebframework.ezorm.rdb.mapping.TestEntity;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.mapping.jpa.JpaEntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.wrapper.EntityResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateResultOperator;
import org.hswebframework.ezorm.rdb.supports.h2.H2SchemaMetadata;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import reactor.util.context.Context;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.IntSupplier;

import static org.junit.Assert.*;

public class SyncCrudEventContextIntegrationTest {

    private static final ContextKey<Map<String, String>> BEFORE_ROWS = ContextKey.of("test-before-rows");

    private Connection connection;
    private Connection observer;
    private RDBTableMetadata table;
    private DefaultDatabaseOperator operator;
    private DefaultSyncRepository<TestEntity, String> repository;

    @Before
    public void init() throws SQLException {
        String url = "jdbc:h2:mem:sync_events_" + UUID.randomUUID();
        connection = DriverManager.getConnection(url, "sa", "");
        observer = DriverManager.getConnection(url, "sa", "");
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(Dialect.H2);
        H2SchemaMetadata schema = new H2SchemaMetadata("PUBLIC");
        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(new TestSyncSqlExecutor(new ConnectionProvider() {
            @Override
            public Connection getConnection() {
                return connection;
            }

            @Override
            public void releaseConnect(Connection ignored) {
                // The fixture owns the real connection, including transaction completion.
            }
        }));
        operator = DefaultDatabaseOperator.of(metadata);
        operator.ddl().createOrAlter("entity_test")
            .addColumn("id").primaryKey().varchar(32).commit()
            .addColumn("name").varchar(32).commit()
            .addColumn("state").number(4).commit()
            .addColumn("create_time").alias("createTime").datetime().commit()
            .commit().sync();

        JpaEntityTableMetadataParser parser = new JpaEntityTableMetadataParser();
        parser.setDatabaseMetadata(metadata);
        table = parser.parseTableMetadata(TestEntity.class).orElseThrow(AssertionError::new);
        schema.addTable(table);
        EntityResultWrapper<TestEntity> wrapper = new EntityResultWrapper<>(TestEntity::new);
        wrapper.setMapping(table.<EntityColumnMapping>getFeature(
            MappingFeatureType.columnPropertyMapping.createFeatureId(TestEntity.class))
            .orElseThrow(AssertionError::new));
        repository = new DefaultSyncRepository<>(operator, table, TestEntity.class, wrapper);
        repository.insert(entity("target", "before"));
        repository.insert(entity("survivor", "untouched"));
    }

    @After
    public void close() throws SQLException {
        if (observer != null) {
            observer.close();
        }
        if (connection != null) {
            connection.close();
        }
    }

    @Test
    public void updateSharesDefaultsAndBeforeSnapshotWithAfterSql() {
        Probe probe = listen((type, context) -> {
            if (type == MappingEventTypes.update_before) {
                assertEquals(originalRows(), snapshot(connection));
            } else {
                assertEquals(Map.of("target", "after", "survivor", "untouched"), snapshot(connection));
                assertEquals(originalRows(), context.get(BEFORE_ROWS).orElseThrow(AssertionError::new));
            }
        });

        assertEquals(1, repository.createUpdate().set("name", "after").where("id", "target").execute());

        assertSuccess(probe, 1);
        assertEquals(Map.of("target", "after", "survivor", "untouched"), snapshot(observer));
    }

    @Test
    public void deleteSharesDefaultsAndBeforeSnapshotWithAfterSql() {
        Probe probe = listen((type, context) -> {
            if (type == MappingEventTypes.delete_before) {
                assertEquals(originalRows(), snapshot(connection));
            } else {
                assertEquals(Map.of("survivor", "untouched"), snapshot(connection));
                assertEquals(originalRows(), context.get(BEFORE_ROWS).orElseThrow(AssertionError::new));
            }
        });

        assertEquals(1, repository.createDelete().where("id", "target").execute());

        assertSuccess(probe, 1);
        assertEquals(Map.of("survivor", "untouched"), snapshot(observer));
    }

    @Test
    public void singleAndBatchInsertAndSaveUseSameExecutionContext() {
        for (String operation : List.of("insert-single", "insert-batch", "save-single", "save-batch")) {
            Map<String, String> beforeRows = snapshot(connection);
            Map<String, String> expectedRows = new LinkedHashMap<>(beforeRows);
            if (operation.startsWith("insert")) {
                expectedRows.put(operation + "-1", "inserted first");
                if (operation.endsWith("batch")) {
                    expectedRows.put(operation + "-2", "inserted second");
                }
            } else {
                expectedRows.put("target", operation);
                if (operation.endsWith("batch")) {
                    expectedRows.put("saved-new", "saved new");
                }
            }
            Probe probe = listen((type, context) -> {
                assertEquals(operation.equals("insert-single") ? "single" : "batch",
                             context.get(MappingContextKeys.type).orElse(null));
                assertNotNull(context.get(MappingContextKeys.instance).orElse(null));
                if (isBefore(type)) {
                    assertEquals(beforeRows, snapshot(connection));
                } else {
                    assertEquals(expectedRows, snapshot(connection));
                    assertEquals(beforeRows, context.get(BEFORE_ROWS).orElseThrow(AssertionError::new));
                }
            });

            Object result;
            switch (operation) {
                case "insert-single":
                    repository.insert(entity(operation + "-1", "inserted first"));
                    result = 1;
                    break;
                case "insert-batch":
                    result = repository.insertBatch(List.of(entity(operation + "-1", "inserted first"),
                                                             entity(operation + "-2", "inserted second")));
                    assertEquals(2, result);
                    break;
                case "save-single":
                    SaveResult saved = repository.save(entity("target", operation));
                    assertEquals(0, saved.getAdded());
                    assertEquals(1, saved.getUpdated());
                    result = saved;
                    break;
                default:
                    SaveResult batch = repository.save(List.of(entity("target", operation),
                                                                entity("saved-new", "saved new")));
                    assertEquals(1, batch.getAdded());
                    assertEquals(1, batch.getUpdated());
                    result = batch;
                    break;
            }

            assertEquals(1, probe.before.size());
            assertEquals(1, probe.after.size());
            assertTrue(probe.errors.isEmpty());
            assertSame(probe.before.get(0), probe.after.get(0));
            Object eventResult = probe.after.get(0).get(MappingContextKeys.result).orElse(null);
            if (result instanceof SaveResult) {
                assertSame(result, eventResult);
            } else {
                assertEquals(result, eventResult);
            }
            assertEquals(expectedRows, snapshot(observer));
        }
    }

    @Test
    public void updateListenerFailuresRollBackSqlAndCallerWrites() throws SQLException {
        assertListenerFailuresRollBack(false);
    }

    @Test
    public void deleteListenerFailuresRollBackSqlAndCallerWrites() throws SQLException {
        assertListenerFailuresRollBack(true);
    }

    private void assertListenerFailuresRollBack(boolean delete) throws SQLException {
        for (boolean rejectBefore : List.of(true, false)) {
            IllegalStateException expected = new IllegalStateException("reject " + (rejectBefore ? "before" : "after"));
            Probe probe = listen((type, context) -> {
                if (context.get(MappingContextKeys.error).isPresent()) {
                    assertSame(expected, context.get(MappingContextKeys.error).orElseThrow(AssertionError::new));
                    return;
                }
                boolean before = isBefore(type);
                if (before == rejectBefore) {
                    String expectedName = before ? "before" : delete ? null : "after";
                    assertEquals(expectedName, snapshot(connection).get("target"));
                    throw expected;
                }
            });

            IllegalStateException actual = assertThrows(IllegalStateException.class,
                () -> inTransaction(() -> {
                    callerWrites();
                    if (delete) {
                        repository.createDelete().where("id", "target").execute();
                    } else {
                        repository.createUpdate().set("name", "after").where("id", "target").execute();
                    }
                }));

            assertSame(expected, actual);
            assertEquals(1, probe.before.size());
            assertEquals(1, probe.errors.size());
            assertSame(probe.before.get(0), probe.errors.get(0));
            assertEquals(rejectBefore ? 0 : 1, probe.after.size());
            assertEquals(rejectBefore, probe.errors.get(0).get(MappingContextKeys.result).isEmpty());
            assertEquals(originalRows(), snapshot(connection));
            assertEquals(originalRows(), snapshot(observer));
        }
    }

    @Test
    public void sqlFailureSharesBeforeContextAndRollsBackCallerWrites() throws SQLException {
        Probe probe = listen((type, context) -> { });
        SQLException rejected = assertThrows(SQLException.class, () -> inTransaction(() -> {
            callerWrites();
            repository.createUpdate().set("name", "name exceeds the mapped thirty two character column")
                .where("id", "target").execute();
        }));

        assertEquals(1, probe.before.size());
        assertTrue(probe.after.isEmpty());
        assertEquals(1, probe.errors.size());
        EventContext error = probe.errors.get(0);
        assertSame(probe.before.get(0), error);
        Throwable sqlFailure = error.get(MappingContextKeys.error).orElseThrow(AssertionError::new);
        assertSame(sqlFailure, rejected);
        assertEquals("22001", rejected.getSQLState());
        assertTrue(error.get(MappingContextKeys.result).isEmpty());
        assertEquals(originalRows(), snapshot(connection));
        assertEquals(originalRows(), snapshot(observer));
    }

    @Test
    public void errorNotificationFailurePreservesOriginalAndRollback() throws SQLException {
        IllegalStateException primary = new IllegalStateException("reject after SQL");
        IllegalArgumentException secondary = new IllegalArgumentException("reject error notification");
        Probe probe = listen((type, context) -> {
            if (context.get(MappingContextKeys.error).isPresent()) {
                assertSame(primary, context.get(MappingContextKeys.error).orElseThrow(AssertionError::new));
                throw secondary;
            }
            if (!isBefore(type)) {
                assertEquals("after", snapshot(connection).get("target"));
                throw primary;
            }
        });

        IllegalStateException rejected = assertThrows(IllegalStateException.class, () -> inTransaction(() -> {
            callerWrites();
            repository.createUpdate().set("name", "after").where("id", "target").execute();
        }));

        assertSame(primary, rejected);
        assertArrayEquals(new Throwable[]{secondary}, rejected.getSuppressed());
        assertSame(probe.before.get(0), probe.after.get(0));
        assertSame(probe.before.get(0), probe.errors.get(0));
        assertEquals(originalRows(), snapshot(observer));
    }

    @Test
    public void repeatedUpdateDslAndResultOperatorHaveIndependentContexts() throws SQLException {
        for (boolean resultOperator : List.of(false, true)) {
            Probe probe = listen((type, context) -> { });
            DefaultSyncUpdate<TestEntity> update = (DefaultSyncUpdate<TestEntity>) repository.createUpdate();
            update.set("name", "after").where("id", "target");
            UpdateResultOperator result = update.doExecute();
            IntSupplier execute = resultOperator ? () -> result.sync() : update::execute;

            assertEquals(1, execute.getAsInt());
            assertEquals(1, execute.getAsInt());

            assertIndependentExecutions(probe, 1, 1);
            assertEquals("before", probe.before.get(0).get(BEFORE_ROWS).orElseThrow(AssertionError::new).get("target"));
            assertEquals("after", probe.before.get(1).get(BEFORE_ROWS).orElseThrow(AssertionError::new).get("target"));
            sql("UPDATE entity_test SET name='before' WHERE id='target'");
        }
    }

    @Test
    public void repeatedDeleteDslAndResultOperatorHaveIndependentContexts() throws SQLException {
        for (boolean resultOperator : List.of(false, true)) {
            Probe probe = listen((type, context) -> { });
            DefaultSyncDelete delete = (DefaultSyncDelete) repository.createDelete();
            delete.where("id", "target");
            DeleteResultOperator result = delete.doExecute();
            IntSupplier execute = resultOperator ? () -> result.sync() : delete::execute;

            assertEquals(1, execute.getAsInt());
            assertEquals(0, execute.getAsInt());

            assertIndependentExecutions(probe, 1, 0);
            assertEquals(originalRows(), probe.before.get(0).get(BEFORE_ROWS).orElseThrow(AssertionError::new));
            assertEquals(Map.of("survivor", "untouched"), probe.before.get(1).get(BEFORE_ROWS).orElseThrow(AssertionError::new));
            sql("INSERT INTO entity_test(id,name) VALUES ('target','before')");
        }
    }

    @Test
    public void reusedResultOperatorDoesNotRetainFailedExecutionContext() throws SQLException {
        IllegalStateException rejection = new IllegalStateException("reject first execution");
        Probe probe = listen((type, context) -> {
            if (!isBefore(type) && context.get(MappingContextKeys.error).isEmpty() && probeBeforeCount(context) == 1) {
                throw rejection;
            }
        });
        DefaultSyncUpdate<TestEntity> update = (DefaultSyncUpdate<TestEntity>) repository.createUpdate();
        update.set("name", "after").where("id", "target");
        UpdateResultOperator result = update.doExecute();

        assertSame(rejection, assertThrows(IllegalStateException.class, () -> inTransaction(() -> {
            callerWrites();
            result.sync();
        })));
        assertEquals(originalRows(), snapshot(observer));
        assertEquals(Integer.valueOf(1), result.sync());

        assertEquals(2, probe.before.size());
        assertNotSame(probe.before.get(0), probe.before.get(1));
        assertSame(probe.before.get(0), probe.errors.get(0));
        assertTrue(probe.before.get(1).get(MappingContextKeys.error).isEmpty());
        assertEquals(Map.of("target", "after", "survivor", "untouched"), snapshot(observer));
    }

    @Test
    public void publishedSyncConstructorsStillExecuteRealSql() {
        SyncUpdate<TestEntity> update = new DefaultSyncUpdate<>(table, operator.dml().update(table),
                                                               repository.getMapping(), Context.empty());
        assertEquals(1, update.set("name", "after").where("id", "target").execute());
        SyncDelete delete = new DefaultSyncDelete(table, operator.dml().delete(table), Context.empty());
        assertEquals(1, delete.where("id", "target").execute());
        assertEquals(Map.of("survivor", "untouched"), snapshot(observer));
    }

    private Probe listen(BiConsumer<EventType, EventContext> action) {
        Probe probe = new Probe(action);
        table.addFeature(probe);
        return probe;
    }

    private final class Probe implements EventListener {
        private final List<EventContext> before = new ArrayList<>();
        private final List<EventContext> after = new ArrayList<>();
        private final List<EventContext> errors = new ArrayList<>();
        private final BiConsumer<EventType, EventContext> action;

        private Probe(BiConsumer<EventType, EventContext> action) {
            this.action = action;
        }

        @Override
        public void onEvent(EventType type, EventContext context) {
            if (type != MappingEventTypes.update_before && type != MappingEventTypes.update_after
                && type != MappingEventTypes.delete_before && type != MappingEventTypes.delete_after
                && type != MappingEventTypes.insert_before && type != MappingEventTypes.insert_after
                && type != MappingEventTypes.save_before && type != MappingEventTypes.save_after) {
                return;
            }
            assertSame(repository, context.get(MappingContextKeys.repository).orElse(null));
            assertSame(repository.getMapping(), context.get(MappingContextKeys.columnMapping).orElse(null));
            assertSame(operator, context.get(ContextKeys.database).orElse(null));
            assertSame(table, context.get(ContextKeys.table).orElse(null));
            assertEquals(Boolean.FALSE, context.get(MappingContextKeys.reactive).orElse(null));
            if (type == MappingEventTypes.insert_before || type == MappingEventTypes.insert_after) {
                assertNotNull(context.get(MappingContextKeys.insert).orElse(null));
            } else if (type == MappingEventTypes.save_before || type == MappingEventTypes.save_after) {
                assertNotNull(context.get(MappingContextKeys.upsert).orElse(null));
            } else {
                assertNotNull(context.get(ContextKeys.source).orElse(null));
            }
            if (isBefore(type)) {
                assertTrue(context.get(BEFORE_ROWS).isEmpty());
                assertTrue(context.get(MappingContextKeys.result).isEmpty());
                assertTrue(context.get(MappingContextKeys.error).isEmpty());
                context.set(BEFORE_ROWS, snapshot(connection));
                before.add(context);
                context.set("test-execution", before.size());
            } else {
                assertFalse(context.get(BEFORE_ROWS).isEmpty());
                (context.get(MappingContextKeys.error).isPresent() ? errors : after).add(context);
            }
            action.accept(type, context);
        }
    }

    private static int probeBeforeCount(EventContext context) {
        return (Integer) context.get("test-execution");
    }

    private static boolean isBefore(EventType type) {
        return type == MappingEventTypes.update_before || type == MappingEventTypes.delete_before
            || type == MappingEventTypes.insert_before || type == MappingEventTypes.save_before;
    }

    private static void assertSuccess(Probe probe, int result) {
        assertEquals(1, probe.before.size());
        assertEquals(1, probe.after.size());
        assertTrue(probe.errors.isEmpty());
        assertSame(probe.before.get(0), probe.after.get(0));
        assertEquals(result, probe.after.get(0).get(MappingContextKeys.result).orElse(null));
    }

    private static void assertIndependentExecutions(Probe probe, int first, int second) {
        assertEquals(2, probe.before.size());
        assertEquals(2, probe.after.size());
        assertTrue(probe.errors.isEmpty());
        assertNotSame(probe.before.get(0), probe.before.get(1));
        assertSame(probe.before.get(0), probe.after.get(0));
        assertSame(probe.before.get(1), probe.after.get(1));
        assertEquals(first, probe.after.get(0).get(MappingContextKeys.result).orElse(null));
        assertEquals(second, probe.after.get(1).get(MappingContextKeys.result).orElse(null));
    }

    private void inTransaction(SqlAction action) throws SQLException {
        connection.setAutoCommit(false);
        try {
            action.run();
            connection.commit();
        } catch (SQLException | RuntimeException | Error error) {
            connection.rollback();
            throw error;
        } finally {
            connection.setAutoCommit(true);
        }
    }

    private void callerWrites() throws SQLException {
        sql("INSERT INTO entity_test(id,name) VALUES ('caller','must roll back')");
        sql("UPDATE entity_test SET name='caller changed' WHERE id='survivor'");
    }

    private void sql(String sql) throws SQLException {
        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.executeUpdate();
        }
    }

    private static Map<String, String> snapshot(Connection connection) {
        try (PreparedStatement statement = connection.prepareStatement("SELECT id,name FROM entity_test");
             ResultSet rows = statement.executeQuery()) {
            Map<String, String> result = new LinkedHashMap<>();
            while (rows.next()) {
                result.put(rows.getString(1), rows.getString(2));
            }
            return result;
        } catch (SQLException error) {
            throw new AssertionError(error);
        }
    }

    private static Map<String, String> originalRows() {
        return Map.of("target", "before", "survivor", "untouched");
    }

    private static TestEntity entity(String id, String name) {
        TestEntity entity = new TestEntity();
        entity.setId(id);
        entity.setName(name);
        return entity;
    }

    private interface SqlAction {
        void run() throws SQLException;
    }
}
