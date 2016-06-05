package og.hsweb.ezorm.run;

import og.hsweb.ezorm.param.SqlParam;

import java.sql.SQLException;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Delete {
    Delete where(String condition, Object value);

    Delete setParam(SqlParam param);

    int exec() throws SQLException;
}
