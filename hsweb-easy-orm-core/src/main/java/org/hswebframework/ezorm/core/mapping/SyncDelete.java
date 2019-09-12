package org.hswebframework.ezorm.core.mapping;

import org.hswebframework.ezorm.core.Conditional;

public interface SyncDelete extends Conditional<SyncDelete> {


    int execute();
}
