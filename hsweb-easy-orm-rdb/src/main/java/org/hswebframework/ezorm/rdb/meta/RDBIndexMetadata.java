package org.hswebframework.ezorm.rdb.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author zhouhao
 * @since 1.0.0
 */
@Getter
@Setter
public class RDBIndexMetadata implements ObjectMetadata {

    private String indexName;

    private String alias;

    private Set<IndexColumn> columnName = new LinkedHashSet<>();

    private boolean unique;

    public boolean contains(String column) {
        return columnName.stream().anyMatch(indexColumn -> indexColumn.getColumn().equals(column));
    }

    @Override
    public String getName() {
        return indexName;
    }

    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.index;
    }

    @Getter
    @Setter
    public static class IndexColumn implements Serializable {
        private String column;

        private String sort;

        public static IndexColumn of(String column, String sort) {
            IndexColumn indexColumn = new IndexColumn();
            indexColumn.setColumn(column);
            indexColumn.setSort(sort);
            return indexColumn;
        }
    }
}
