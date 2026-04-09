package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;

import java.util.Map;

@Getter
public class ValueByTimeFunctionFragmentBuilder implements FunctionFragmentBuilder {

    //必须是 TIMESTAMP（时间戳）或 INTEGER（整数）类型 字段
    public static final String TIME_COLUMN = "time";

    private final String function;

    private final String name;

    private final SqlFragments FUNCTION;

    public ValueByTimeFunctionFragmentBuilder(String function, String alias, String name) {
        this.function = function;
        this.name = name;
        FUNCTION = SqlFragments.single(alias + "(");
    }

    public ValueByTimeFunctionFragmentBuilder(String function, String name) {
        this.function = function;
        this.name = name;
        FUNCTION = SqlFragments.single(function + "(");
    }


    @Override
    public SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts) {
        String table = metadata.getOwner().getName();
        String fullTimeColumn;
        if (opts != null && opts.containsKey(TIME_COLUMN)) {
            String timeColumn = (String) opts.get(TIME_COLUMN);
            fullTimeColumn = metadata.getDialect().buildColumnFullName(table, timeColumn);
        } else {
            fullTimeColumn = metadata.getDialect().buildColumnFullName(table, "timestamp");
        }
        if (columnFullName == null) {
            return EmptySqlFragments.INSTANCE;
        }
        return new BatchSqlFragments(2, 0)
            .add(FUNCTION)
            .addSql(columnFullName, ",", fullTimeColumn, ")");
    }


}
