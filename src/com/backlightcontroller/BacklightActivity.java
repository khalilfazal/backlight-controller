package com.backlightcontroller;

import java.net.URI;

import org.apache.http.HttpHost;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView.BufferType;

/**
 * The Class BacklightActivity.
 */
public class BacklightActivity extends Activity {

    /**
     * The Constant PREFS_NAME.
     */
    private static final String PREFS_NAME = "Backlight Controller";

    /**
     * The Constant brightness.
     */
    private static final String _brightness = "brightness";

    /**
     * The Constant hostname.
     */
    private static final String _hostname = "hostname";

    /**
     * The seek bar.
     */
    private SeekBar seekBar;

    /**
     * The max.
     */
    private int max;

    /**
     * The hostname.
     */
    private String hostname;

    /**
     * The preferences.
     */
    protected SharedPreferences preferences;

    /**
     * On create.
     *
     * @param savedInstanceState the saved instance state
     * @see android.app.Activity#onCreate(android.os.Bundle)
     */
    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.setContentView(R.layout.main);
        this.preferences = this.getSharedPreferences(PREFS_NAME, 0);
        this.seekBar = (SeekBar) this.findViewById(R.id.seekBar);
        this.max = this.seekBar.getMax();
    }

    /**
     * On resume.
     *
     * @see android.app.Activity#onResume()
     */
    @Override
    protected void onResume() {
        super.onResume();
        this.seekBar.setProgress(this.preferences.getInt(_brightness, this.max));
        this.addListener(this.preferences.getString(_hostname, ""));
    }

    /**
     * Adds the listener.
     *
     * @param hostname the hostname
     */
    protected void addListener(final String hostname) {
        if (this.validHostname(hostname)) {
            this.hostname = hostname;
            final OnSeekBarChangeListener listener = new BacklightAdjuster(this.hostname, this.max);
            this.seekBar.setOnSeekBarChangeListener(listener);
        } else {
            final EditText input = new EditText(this);
            input.setHint(R.string.hostnameHint);
            input.setText(hostname == "" ? this.hostname : hostname, BufferType.EDITABLE);

            final OnClickListener listener = new OnClickListener() {

                @Override
                public void onClick(final DialogInterface dialog, final int which) {
                    BacklightActivity.this.addListener(input.getText().toString());
                }
            };

            new AlertDialog.Builder(this)
                    .setTitle(R.string.hostnameTitle)
                    .setMessage(R.string.hostnameMessage)
                    .setView(input)
                    .setNeutralButton(R.string.hostnameSet, listener)
                    .show();
        }
    }

    /**
     * Valid hostname.
     *
     * @param hostname the hostname
     * @return true, if successful
     */
    @SuppressWarnings("unused")
    private boolean validHostname(final String hostname) {
        try {
            final URI requestURI = new URI("http://" + hostname);
            new HttpHost(
                    requestURI.getHost(),
                    requestURI.getPort(),
                    requestURI.getScheme());
        } catch (final Exception e) {
            return false;
        }

        return true;
    }

    /**
     * On pause.
     *
     * @see android.app.Activity#onPause()
     */
    @Override
    protected void onPause() {
        super.onPause();

        final Editor editor = this.preferences.edit();
        editor.putInt(_brightness, this.seekBar.getProgress());
        editor.putString(_hostname, this.hostname);
        editor.commit();
    }

    /**
     * On create options menu.
     *
     * @param menu the menu
     * @return true, if successful
     * @see android.app.Activity#onCreateOptionsMenu(android.view.Menu)
     */
    @Override
    public boolean onCreateOptionsMenu(final Menu menu) {
        this.getMenuInflater().inflate(R.menu.menu, menu);
        return super.onCreateOptionsMenu(menu);
    }

    /**
     * On options item selected.
     *
     * @param item the item
     * @return true, if successful
     * @see android.app.Activity#onOptionsItemSelected(android.view.MenuItem)
     */
    @Override
    public boolean onOptionsItemSelected(final MenuItem item) {
        switch (item.getItemId()) {
            case R.id.hostname:
                this.addListener("");
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}