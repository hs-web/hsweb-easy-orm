package org.hswebframework.ezorm.rdb.metadata;

import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.core.param.Term;

import java.util.List;

public interface ForeignKeyMetadata extends ObjectMetadata {

    @Override
    default ObjectType getObjectType() {
        return RDBObjectType.foreign_key;
    }

    /**
     * @return 是否为逻辑外键
     */
    boolean isLogical();

    /**
     * @return 是否n对多
     */
    boolean isToMany();

    TableOrViewMetadata getSource();

    TableOrViewMetadata getTarget();

    RDBColumnMetadata getSourceColumn();

    RDBColumnMetadata getTargetColumn();

    List<Term> getTerms();
}
