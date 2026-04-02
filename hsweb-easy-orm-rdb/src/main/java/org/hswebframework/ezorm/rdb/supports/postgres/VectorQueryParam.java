package org.hswebframework.ezorm.rdb.supports.postgres;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import org.apache.commons.beanutils.BeanUtils;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.NativeSql;

import java.util.Map;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
public class VectorQueryParam {

    /**
     * 求相似距离的向量值
     */
    private Float[] vector;
    /**
     * 和相似距离比较的操作符
     */
    private String termType = TermType.lte;
    /**
     * 距离 （0~1） 0最相似
     */
    private float distance = 0.25f;

    //该l2模型距离分布的中位数
    private int alpha = 50;

    public VectorQueryParam(Object vector) {
        this.vector = VectorType.toFloatArray(vector);
    }

    @JsonIgnore
    public Term getTerm(VectorTermType type, String column) {
        return Term.of(column, termType, NativeSql.of("?", type.toSqlValue(alpha, distance)));
    }

    @SneakyThrows
    public static VectorQueryParam of(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof VectorQueryParam) {
            return ((VectorQueryParam) value);
        }
        if (value instanceof Map<?, ?> v) {
            VectorQueryParam term = new VectorQueryParam();
            BeanUtils.copyProperties(v, term);
            if (term.getVector() == null) {
                term.setVector(VectorType.toFloatArray(v.get("vector")));
            }
            return term;
        }
        return new VectorQueryParam(value);
    }

}
