package org.hsweb.ezorm.run;

import org.hsweb.ezorm.param.SqlParam;

import java.sql.SQLException;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Delete extends TriggerSkipSupport<Delete>{
    Delete where(String condition, Object value);

    Delete setParam(SqlParam param);

    int exec() throws SQLException;
}
