package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;

public interface TermFragment {

    PrepareSqlFragment createFragments(String tableName, RDBColumnMetadata column, Term term);

}
