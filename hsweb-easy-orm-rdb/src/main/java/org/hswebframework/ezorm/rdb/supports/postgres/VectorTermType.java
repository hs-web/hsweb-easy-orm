package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum VectorTermType {
    /**
     * L2 Distance，不推荐使用
     * [0,正无穷]，0最相似
     */
    vector_l2("<->") {
        @Override
        public Float toSqlValue(int alpha, Float value) {
            return value * alpha / (1 - value);
        }
    },
    /**
     * cosine distance
     * [0,2]，0最相似
     */
    vector_cos("<=>") {
        @Override
        public Float toSqlValue(int alpha, Float value) {
            return 2 * value;
        }
    },
    /**
     * Inner Product 归一化后
     * [-1,1]，-1最相似
     */
    vector_ip("<#>") {
        @Override
        public Float toSqlValue(int alpha, Float value) {
            if (value == 0) {
                return -1f;
            }
            return 2 * value - 1;
        }
    };

    private final String operation;

    /**
     * 将统一 distance（0~1）转换为 SQL 条件值
     *
     * @param alpha 该l2模型距离分布的中位数
     * @param distance 距离 （0~1） 0最相似
     * @return 值
     */
    public abstract Float toSqlValue(int alpha, Float distance);
}