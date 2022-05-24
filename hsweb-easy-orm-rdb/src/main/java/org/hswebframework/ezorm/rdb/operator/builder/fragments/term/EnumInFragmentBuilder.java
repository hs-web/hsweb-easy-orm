package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import lombok.Getter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerDialect;
import org.hswebframework.ezorm.rdb.supports.mssql.SqlServerEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlDialect;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleDialect;
import org.hswebframework.ezorm.rdb.supports.oracle.OracleEnumInFragmentBuilder;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlDialect;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlEnumInFragmentBuilder;

import java.util.List;

public abstract class EnumInFragmentBuilder extends AbstractTermFragmentBuilder {

    @Getter
    private final boolean not;

    public EnumInFragmentBuilder(boolean not) {
        super(not ? TermType.nin : TermType.in, "枚举In");
        this.not = not;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        List<Object> values = convertList(column, term);
        long mask = 0;
        boolean any = term.getOptions().contains("any");
        for (Object value : values) {
            if (value instanceof Number) {
                mask |= ((Number) value).longValue();
            } else if (value instanceof Enum) {
                mask |= 1L << ((Enum<?>) value).ordinal();
            }
        }

        PrepareSqlFragments sql = bitAnd(columnFullName, mask);

        if (any) {
            // arr & 10 != 0
            sql.addSql(not ? "=" : "!=", "0");
        } else {
            // arr & 10 = arr
            sql.addSql(not ? "!=" : "=", columnFullName);
        }

        return sql;
    }

    public abstract PrepareSqlFragments bitAnd(String column, long value);

    public static EnumInFragmentBuilder of(Dialect dialect) {
        if (dialect instanceof MysqlDialect) {
            return MysqlEnumInFragmentBuilder.in;
        }
        if (dialect instanceof PostgresqlDialect) {
            return PostgresqlEnumInFragmentBuilder.in;
        }
        if (dialect instanceof SqlServerDialect) {
            return SqlServerEnumInFragmentBuilder.in;
        }
        if (dialect instanceof OracleDialect) {
            return OracleEnumInFragmentBuilder.in;
        }

        throw new UnsupportedOperationException("unsupported db type :" + dialect.getType());
    }
}
