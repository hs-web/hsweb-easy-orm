package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.param.QueryParam;

public interface DSLDelete<ME extends DSLDelete> extends Conditional<ME> {

     QueryParam toQueryParam();


}
