/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.rdb.meta.converter;

import org.hsweb.ezorm.core.ValueConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.sql.rowset.serial.SerialBlob;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.sql.Blob;
import java.sql.SQLException;

public class BlobValueConverter implements ValueConverter {
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    @Override
    public Object getData(Object value) {
        if (value instanceof String) {
            value = ((String) value).getBytes();
        }
        if (value instanceof byte[]) {
            try {
                return new SerialBlob(((byte[]) value));
            } catch (SQLException e) {
                return value;
            }
        }
        return value;
    }

    @Override
    public Object getValue(Object data) {
        if (data == null) return null;
        if (data instanceof Blob) {
            Blob blobValue = ((Blob) data);
            try (InputStream inputStream = blobValue.getBinaryStream()) {
                //尝试转为对象
                try {
                    ObjectInputStream inputStream1 = new ObjectInputStream(inputStream);
                    return inputStream1.readObject();
                } catch (IOException e) {
                    //可能不是对象
                } catch (ClassNotFoundException e) {
                    logger.warn("blob is class,but class not found!", e);
                }
                //转为bytes
                return blobValue.getBytes(0, (int) blobValue.length());
            } catch (Exception e) {
                logger.warn("blob data error", e);
            }
        }
        return data;
    }
}
