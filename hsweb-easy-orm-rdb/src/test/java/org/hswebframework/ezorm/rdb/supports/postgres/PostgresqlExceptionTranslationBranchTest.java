package org.hswebframework.ezorm.rdb.supports.postgres;

import io.r2dbc.postgresql.api.ErrorDetails;
import io.r2dbc.postgresql.api.PostgresqlException;
import io.r2dbc.postgresql.message.backend.Field;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class PostgresqlExceptionTranslationBranchTest {

    @Test
    public void testDuplicateKeyBranches() {
        RDBDatabaseMetadata db = new RDBDatabaseMetadata(Dialect.POSTGRES);
        PostgresqlSchemaMetadata schema = new PostgresqlSchemaMetadata("public");
        db.addSchema(schema); db.setCurrentSchema(schema);
        RDBTableMetadata table = schema.newTable("t");
        RDBColumnMetadata id = table.newColumn(); id.setName("id"); id.setPrimaryKey(true); table.addColumn(id);
        schema.addTable(table);
        PostgresqlR2DBCExceptionTranslation translation = PostgresqlR2DBCExceptionTranslation.of(schema);

        ErrorDetails details = new ErrorDetails(List.of(
            new Field(Field.FieldType.CODE, "23505"),
            new Field(Field.FieldType.TABLE_NAME, "t"),
            new Field(Field.FieldType.CONSTRAINT_NAME, "pk_t")
        ));
        Throwable mapped = translation.translate(new TestPostgresqlException(details));
        Assert.assertTrue(mapped instanceof DuplicateKeyException);

        ErrorDetails fallbackDetails = new ErrorDetails(List.of(
            new Field(Field.FieldType.CODE, "23505"),
            new Field(Field.FieldType.TABLE_NAME, "t"),
            new Field(Field.FieldType.CONSTRAINT_NAME, "missing")
        ));
        Throwable fallback = translation.translate(new TestPostgresqlException(fallbackDetails));
        Assert.assertTrue(fallback instanceof DuplicateKeyException);

        RuntimeException plain = new RuntimeException("plain");
        Assert.assertSame(plain, translation.translate(plain));
    }

    static class TestPostgresqlException extends RuntimeException implements PostgresqlException {
        private final ErrorDetails details;
        TestPostgresqlException(ErrorDetails details) { this.details = details; }
        @Override public ErrorDetails getErrorDetails() { return details; }
    }
}
