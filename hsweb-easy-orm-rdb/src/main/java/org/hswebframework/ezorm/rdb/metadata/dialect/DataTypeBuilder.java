package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

public interface DataTypeBuilder {
    String createColumnDataType(RDBColumnMetadata columnMetaData);

    /**
     * Resolves numeric dimensions and defaults without changing column metadata.
     * Unsupported unconstrained declarations fail instead of silently fixing their precision.
     * @since 4.2.4
     */
    static DataTypeBuilder numeric(String name, int defaultPrecision, boolean supportsUnconstrained) {
        return column -> {
            if (column.isUnconstrainedNumeric()) {
                if (!supportsUnconstrained) {
                    throw new UnsupportedOperationException("unconstrained numeric is unsupported for " + name);
                }
                return name;
            }
            return name + "(" + column.getPrecision(defaultPrecision) + "," + column.getScale() + ")";
        };
    }
}
