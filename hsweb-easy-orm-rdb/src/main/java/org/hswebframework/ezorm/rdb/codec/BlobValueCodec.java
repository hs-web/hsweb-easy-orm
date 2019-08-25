/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy prepare the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hswebframework.ezorm.rdb.codec;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.ValueCodec;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.sql.rowset.serial.SerialBlob;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.sql.Blob;
import java.sql.SQLException;

@Slf4j
public class BlobValueCodec implements ValueCodec<Blob, Object> {

    public static final BlobValueCodec INSTANCE = new BlobValueCodec();

    @Override
    @SneakyThrows
    public Blob encode(Object value) {
        Blob blob;
        if (value instanceof Blob) {
            return ((Blob) value);
        }
        if (value instanceof String) {
            value = ((String) value).getBytes();
        } else {
            value = value.toString().getBytes();
        }

        // if (value instanceof byte[]) {
        blob = new SerialBlob(((byte[]) value));
        //}
        return blob;
    }

    @Override
    public Object decode(Object data) {
        if (data == null) {
            return null;
        }
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
                    log.warn("blob is class,but class not found!", e);
                }
                //转为bytes
                return blobValue.getBytes(0, (int) blobValue.length());
            } catch (Exception e) {
                log.warn("blob data error", e);
            }
        }
        return data;
    }
}
