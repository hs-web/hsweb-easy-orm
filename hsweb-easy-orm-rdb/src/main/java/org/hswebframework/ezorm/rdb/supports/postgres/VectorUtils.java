package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;

import java.util.stream.Collectors;
import java.util.stream.Stream;

public class VectorUtils {


    /**
     * 获取向量距离计算字段，例如通过获取到 {@code table_a.embed <=> '[1,1,1]'}
     *
     * @param columnFullName 字段，例如{@code table_a.embed}
     * @param type           类型
     * @param vector         目标向量值
     * @return 向量距离计算字段
     */
    public static String getVectorColumn(String columnFullName, VectorTermType type, Float[] vector) {
        return PrepareSqlFragments
            .of(columnFullName)
            .addSql(type.getOperation(),"?")
            .addParameter(toVectorLiteral(vector))
            .toRequest()
            .toNativeSql();
    }

    public static String toVectorLiteral(Float[] vector) {
        return Stream
            .of(vector)
            .map(String::valueOf)
            .collect(Collectors.joining(",", "[", "]"));
    }

}
