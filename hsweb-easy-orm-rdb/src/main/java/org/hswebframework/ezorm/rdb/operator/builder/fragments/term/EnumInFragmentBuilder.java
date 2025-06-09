package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import lombok.Getter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
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

@Getter
public abstract class EnumInFragmentBuilder extends AbstractTermFragmentBuilder {

    public static SqlFragments NOT_ZERO = SqlFragments.of("!= 0");
    public static SqlFragments IS_ZERO = SqlFragments.of("= 0");

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
        BatchSqlFragments _sql = new BatchSqlFragments(2, 0);
        SqlFragments sql = bitAnd(columnFullName, mask);
        _sql.add(sql);

        if (any) {
            // arr & 10 != 0
            _sql.add(not ? IS_ZERO : NOT_ZERO);
        } else {
            // arr & 10 = arr
            _sql.addSql(not ? "!=" : "=", columnFullName);
        }

        return _sql;
    }

    public abstract SqlFragments bitAnd(String column, long value);

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
        return new EnumInFragmentBuilder(false) {
            @Override
            public SqlFragments bitAnd(String column, long value) {
                return dialect.bitAnd(column,value);
            }
        };
    }

    public static EnumInFragmentBuilder ofNot(Dialect dialect) {
        if (dialect instanceof MysqlDialect) {
            return MysqlEnumInFragmentBuilder.notIn;
        }
        if (dialect instanceof PostgresqlDialect) {
            return PostgresqlEnumInFragmentBuilder.notIn;
        }
        if (dialect instanceof SqlServerDialect) {
            return SqlServerEnumInFragmentBuilder.notIn;
        }
        if (dialect instanceof OracleDialect) {
            return OracleEnumInFragmentBuilder.notIn;
        }
        return new EnumInFragmentBuilder(true) {
            @Override
            public SqlFragments bitAnd(String column, long value) {
                return dialect.bitAnd(column,value);
            }
        };
    }
}
